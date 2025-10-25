import requests
import time
import subprocess
import signal
import os
from typing import Dict, Optional

class PengineController:
    """Controller for multi-user Prolog pengine sessions"""
    
    def __init__(self, port=8080, pengine_server_path="src/pengine_server.pl"):
        self.port = port
        self.pengine_server_path = pengine_server_path
        self.process = None
        self.sessions: Dict[str, str] = {}  # session_id -> pengine_id mapping
        
    def start_server(self, timeout_sec=10):
        """Start the pengine-enabled Prolog server"""
        if self.process:
            raise RuntimeError("Pengine server already running")
            
        try:
            # Start SWI-Prolog with pengine server
            cmd = [
                "swipl", 
                "-g", "start_pengine_server",
                "-t", "halt",
                self.pengine_server_path
            ]
            
            env = os.environ.copy()
            env["PORT"] = str(self.port)
            
            self.process = subprocess.Popen(
                cmd,
                env=env,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE
            )
            
            # print(f"⏳ Starting Pengine server on port {self.port}...")
            
            # Wait for server readiness
            start_time = time.time()
            while (time.time() - start_time) < timeout_sec:
                try:
                    # Try to create a test session to verify server is ready
                    response = requests.post(
                        f"http://localhost:{self.port}/session",
                        json={"action": "create"},
                        timeout=1
                    )
                    if response.status_code == 200:
                        # Cleanup test session
                        data = response.json()
                        requests.post(
                            f"http://localhost:{self.port}/session",
                            json={"action": "destroy", "session_id": data["session_id"]},
                            timeout=1
                        )
                        # print(f"✅ Pengine server ready at http://localhost:{self.port}")
                        return True
                except (requests.ConnectionError, requests.Timeout):
                    time.sleep(0.5)
            
            raise TimeoutError(f"Pengine server didn't start within {timeout_sec} seconds")
            
        except FileNotFoundError:
            raise RuntimeError(
                "SWI-Prolog not found. Please install SWI-Prolog:\n"
                "  Ubuntu/Debian: sudo apt-get install swi-prolog\n"
                "  macOS: brew install swi-prolog\n"
                "  Windows: Download from https://www.swi-prolog.org/download/stable"
            )
    
    def stop_server(self):
        """Stop the pengine server"""
        if self.process:
            try:
                self.process.terminate()
                self.process.wait(timeout=5)
                # print("🛑 Pengine server stopped")
            except subprocess.TimeoutExpired:
                self.process.kill()
                self.process.wait()
                # print("🛑 Pengine server force stopped")
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
            pengine_id = data["pengine_id"]
            self.sessions[session_id] = pengine_id
            
            # print(f"👤 Created session {session_id[:8]}... with pengine {pengine_id[:8]}...")
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
            
            # print(f"🗑️ Destroyed session {session_id[:8]}...")
            return True
            
        except requests.RequestException as e:
            raise ConnectionError(f"Failed to destroy session: {e}")
    
    def add_clause(self, session_id: str, clause_text: str):
        """Add a clause to a specific user session"""
        clause = self._package_clause(clause_text)
        try:
            response = requests.post(
                f"http://localhost:{self.port}/assert",
                json={"session_id": session_id, "clause": clause},
                timeout=5
            )
            response.raise_for_status()
            # print(f"➕ [{session_id[:8]}...] Added clause: {clause}")
            return True
            
        except requests.RequestException as e:
            raise ConnectionError(f"Clause addition failed: {e}")
    
    def remove_clause(self, session_id: str, clause_text: str):
        """Remove a clause from a specific user session"""
        clause = self._package_clause(clause_text)
        try:
            response = requests.post(
                f"http://localhost:{self.port}/retract",
                json={"session_id": session_id, "clause": clause},
                timeout=5
            )
            response.raise_for_status()
            # print(f"➖ [{session_id[:8]}...] Removed clause: {clause}")
            return True
            
        except requests.RequestException as e:
            raise ConnectionError(f"Clause removal failed: {e}")
    
    def get_clauses(self, session_id: str):
        """List all clauses for a specific user session"""
        try:
            response = requests.get(
                f"http://localhost:{self.port}/list_clauses",
                params={"session_id": session_id},
                timeout=5
            )
            response.raise_for_status()
            return response.json()
            
        except requests.RequestException as e:
            raise ConnectionError(f"Failed to fetch clauses: {e}")
    
    def query(self, session_id: str, prolog_query: str, timeout_sec=5):
        """Execute a Prolog query in a specific user session"""
        try:
            response = requests.get(
                f"http://localhost:{self.port}/query",
                params={"session_id": session_id, "q": prolog_query},
                timeout=timeout_sec
            )
            response.raise_for_status()
            return response.json()
            
        except requests.RequestException as e:
            raise ConnectionError(f"Query failed: {e}")
    
    def cleanup_session(self, session_id: str):
        """Cleanup a specific session"""
        try:
            response = requests.post(
                f"http://localhost:{self.port}/cleanup",
                json={"session_id": session_id},
                timeout=5
            )
            response.raise_for_status()
            
            if session_id in self.sessions:
                del self.sessions[session_id]
            
            return True
            
        except requests.RequestException as e:
            raise ConnectionError(f"Session cleanup failed: {e}")
    
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
    print("This module provides PengineController class for multi-user sessions.")
    print("See examples/ folder for usage examples.")