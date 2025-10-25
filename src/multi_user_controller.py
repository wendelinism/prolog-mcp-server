#!/usr/bin/env python3
"""
Controller for the new multi-user Prolog server that provides
the same API as PengineController but with better isolation.
"""

import requests
import time
import subprocess
import signal
import os
from typing import Dict, Optional
import threading

class MultiUserPrologController:
    """Controller for multi-user Prolog server with process-based isolation"""
    
    def __init__(self, port=8080, server_script="src/multi_user_prolog_server.py"):
        self.port = port
        self.server_script = server_script
        self.process = None
        self.sessions: Dict[str, str] = {}  # session_id -> session_id mapping (for compatibility)
        
    def start_server(self, timeout_sec=10):
        """Start the multi-user Prolog server"""
        if self.process:
            raise RuntimeError("Multi-user Prolog server already running")
            
        try:
            # Start the multi-user server
            cmd = ["python3", self.server_script, "--port", str(self.port)]
            
            self.process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                preexec_fn=os.setsid  # Create new process group
            )
            
            # Wait for server readiness
            start_time = time.time()
            while (time.time() - start_time) < timeout_sec:
                try:
                    # Try to create a test session to verify server is ready
                    response = requests.post(
                        f"http://localhost:{self.port}/session",
                        json={"action": "create"},
                        timeout=2
                    )
                    if response.status_code == 200:
                        # Cleanup test session
                        data = response.json()
                        requests.post(
                            f"http://localhost:{self.port}/session",
                            json={"action": "destroy", "session_id": data["session_id"]},
                            timeout=2
                        )
                        return True
                except (requests.ConnectionError, requests.Timeout):
                    time.sleep(0.5)
            
            raise TimeoutError(f"Multi-user server didn't start within {timeout_sec} seconds")
            
        except FileNotFoundError:
            raise RuntimeError("Python3 not found or server script missing")
    
    def stop_server(self):
        """Stop the multi-user Prolog server"""
        if self.process:
            try:
                # Send SIGTERM to the entire process group
                os.killpg(os.getpgid(self.process.pid), signal.SIGTERM)
                self.process.wait(timeout=5)
            except (subprocess.TimeoutExpired, ProcessLookupError):
                try:
                    # Force kill if needed
                    os.killpg(os.getpgid(self.process.pid), signal.SIGKILL)
                    self.process.wait()
                except ProcessLookupError:
                    pass  # Process already dead
            except Exception:
                pass  # Handle any other exceptions gracefully
            finally:
                self.process = None
                self.sessions.clear()
    
    def create_session(self) -> str:
        """Create a new user session and return session ID"""
        try:
            response = requests.post(
                f"http://localhost:{self.port}/session",
                json={"action": "create"},
                timeout=5
            )
            response.raise_for_status()
            data = response.json()
            
            session_id = data["session_id"]
            self.sessions[session_id] = session_id  # Store for tracking
            
            return session_id
            
        except requests.RequestException as e:
            raise ConnectionError(f"Failed to create session: {e}")
    
    def destroy_session(self, session_id: str):
        """Destroy a user session"""
        try:
            response = requests.post(
                f"http://localhost:{self.port}/session",
                json={"action": "destroy", "session_id": session_id},
                timeout=5
            )
            response.raise_for_status()
            
            if session_id in self.sessions:
                del self.sessions[session_id]
            
            return True
            
        except requests.RequestException as e:
            raise ConnectionError(f"Failed to destroy session: {e}")
    
    def add_clause(self, session_id: str, clause_text: str):
        """Add a clause to a specific user session"""
        clause = self._package_clause(clause_text)
        try:
            response = requests.post(
                f"http://localhost:{self.port}/clause",
                json={"session_id": session_id, "clause": clause, "action": "add"},
                timeout=10
            )
            response.raise_for_status()
            return response.json().get('output', 'Success')
            
        except requests.RequestException as e:
            raise ConnectionError(f"Clause addition failed: {e}")
    
    def remove_clause(self, session_id: str, clause_text: str):
        """Remove a clause from a specific user session"""
        clause = self._package_clause(clause_text)
        try:
            response = requests.post(
                f"http://localhost:{self.port}/clause",
                json={"session_id": session_id, "clause": clause, "action": "remove"},
                timeout=5
            )
            response.raise_for_status()
            return response.json().get('output', 'Success')
            
        except requests.RequestException as e:
            raise ConnectionError(f"Clause removal failed: {e}")
    
    def get_clauses(self, session_id: str):
        """List all clauses for a specific user session"""
        try:
            response = requests.get(
                f"http://localhost:{self.port}/clauses",
                params={"session_id": session_id},
                timeout=5
            )
            response.raise_for_status()
            return response.json().get('clauses', [])
            
        except requests.RequestException as e:
            raise ConnectionError(f"Failed to fetch clauses: {e}")
    
    def query(self, session_id: str, prolog_query: str, timeout_sec=10):
        """Execute a Prolog query in a specific user session"""
        try:
            response = requests.post(
                f"http://localhost:{self.port}/query",
                json={"session_id": session_id, "query": prolog_query},
                timeout=timeout_sec
            )
            response.raise_for_status()
            data = response.json()
            return data.get('result', data.get('output', 'No result'))
            
        except requests.RequestException as e:
            raise ConnectionError(f"Query failed: {e}")
    
    def list_sessions(self):
        """List all active sessions"""
        try:
            response = requests.get(
                f"http://localhost:{self.port}/sessions",
                timeout=5
            )
            response.raise_for_status()
            return response.json()
            
        except requests.RequestException as e:
            raise ConnectionError(f"Failed to list sessions: {e}")
    
    def cleanup_session(self, session_id: str):
        """Cleanup a specific session (same as destroy for this implementation)"""
        return self.destroy_session(session_id)
    
    def _package_clause(self, clause_text: str) -> str:
        """Format clause for Prolog interpretation"""
        clause = clause_text.strip()
        if not clause.endswith('.'):
            clause += '.'
        return clause
    
    def __enter__(self):
        """Context manager entry"""
        self.start_server()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - cleanup"""
        self.stop_server()


if __name__ == "__main__":
    print("This module provides MultiUserPrologController class for multi-user sessions.")
    print("Use it as a drop-in replacement for PengineController.")