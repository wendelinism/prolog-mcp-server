#!/usr/bin/env python3
"""
Controller for the working isolated multi-user Prolog server.
This implementation achieves perfect session isolation.
"""

import requests
import time
import subprocess
import signal
import os
from typing import Dict, Optional
import threading

class WorkingIsolatedController:
    """Controller for the working isolated multi-user server"""
    
    def __init__(self, port=8080, server_script="src/simple_isolated_server.py"):
        self.port = port
        self.server_script = server_script
        self.process = None
        self.sessions: Dict[str, str] = {}
        
    def start_server(self, timeout_sec=10):
        """Start the working isolated server"""
        if self.process:
            raise RuntimeError("Server already running")
            
        try:
            cmd = ["python3", self.server_script, "--port", str(self.port)]
            
            self.process = subprocess.Popen(
                cmd,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE,
                preexec_fn=os.setsid
            )
            
            print(f"⏳ Starting working isolated server on port {self.port}...")
            
            # Wait for server readiness
            start_time = time.time()
            while (time.time() - start_time) < timeout_sec:
                try:
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
                        print(f"✅ Working isolated server ready!")
                        return True
                except (requests.ConnectionError, requests.Timeout):
                    time.sleep(0.5)
            
            raise TimeoutError(f"Server didn't start within {timeout_sec} seconds")
            
        except FileNotFoundError:
            raise RuntimeError("Python3 not found or server script missing")
    
    def stop_server(self):
        """Stop the server"""
        if self.process:
            try:
                os.killpg(os.getpgid(self.process.pid), signal.SIGTERM)
                self.process.wait(timeout=5)
                print("🛑 Working isolated server stopped")
            except (subprocess.TimeoutExpired, ProcessLookupError):
                try:
                    os.killpg(os.getpgid(self.process.pid), signal.SIGKILL)
                    self.process.wait()
                    print("🛑 Working isolated server force stopped")
                except ProcessLookupError:
                    pass
            except Exception:
                pass
            finally:
                self.process = None
                self.sessions.clear()
    
    def create_session(self) -> str:
        """Create new session"""
        try:
            response = requests.post(
                f"http://localhost:{self.port}/session",
                json={"action": "create"},
                timeout=5
            )
            response.raise_for_status()
            data = response.json()
            
            session_id = data["session_id"]
            self.sessions[session_id] = session_id
            
            return session_id
            
        except requests.RequestException as e:
            raise ConnectionError(f"Failed to create session: {e}")
    
    def destroy_session(self, session_id: str):
        """Destroy session"""
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
        """Add clause to session"""
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
        """Remove clause from session"""
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
        """Get clauses from session"""
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
        """Execute query in session"""
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
        """List sessions"""
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
        """Cleanup session"""
        return self.destroy_session(session_id)
    
    def _package_clause(self, clause_text: str) -> str:
        """Format clause"""
        clause = clause_text.strip()
        if not clause.endswith('.'):
            clause += '.'
        return clause
    
    def __enter__(self):
        self.start_server()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.stop_server()


if __name__ == "__main__":
    print("Working isolated multi-user controller with PERFECT session isolation!")