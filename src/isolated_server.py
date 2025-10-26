 #!/usr/bin/env python3
"""
Isolated multi-user server with process-based session separation.
"""

import subprocess
import threading
import json
import time
import uuid
import os
import tempfile
import shutil
from typing import Dict, Optional, Tuple

class IsolatedSession:
    """Session with process-based isolation"""
    
    def __init__(self, session_id: str, timeout_sec: int = 300):
        self.session_id = session_id
        self.timeout_sec = timeout_sec
        self.last_activity = time.time()
        self.lock = threading.Lock()
        self.clauses = []

        # Create session directory
        self.session_dir = tempfile.mkdtemp(prefix=f"prolog_isolated_{self.session_id[:8]}_")
    
    def start(self):
        """Initialize session"""
        with self.lock:
            self.last_activity = time.time()
    
    def stop(self):
        """Clean up session"""
        with self.lock:
            if os.path.exists(self.session_dir):
                try:
                    shutil.rmtree(self.session_dir)
                except:
                    pass

    def _create_session_script(self, clauses: list, query: str) -> str:
        """Create isolated Prolog script"""
        query = query.rstrip('.')

        return f"""
% Isolated session {self.session_id[:8]}
% Isolation via separate processes

% Define session clauses
{chr(10).join(clauses)}

% Test query
isolated_test :-
    {query},
    write('TRUE'), nl, !.
isolated_test :-
    write('FALSE'), nl.

% Execute test
:- isolated_test, halt.
"""
    
    def execute_query(self, query: str) -> Tuple[bool, str]:
        """Execute query in isolated process"""
        with self.lock:
            try:
                # Create script file
                script_file = os.path.join(self.session_dir, f"query_{int(time.time()*1000)}.pl")
                
                script_content = self._create_session_script(self.clauses, query)
                
                # Debug: print the generated script for troubleshooting
                print(f"=== Generated Script for {self.session_id[:8]} ===")
                print(script_content)
                print("=" * 50)
                
                with open(script_file, 'w') as f:
                    f.write(script_content)
                
                try:
                    # Execute with simple SWI-Prolog call
                    result = subprocess.run([
                        'swipl', 
                        '-q',
                        '-t', 'halt',
                        script_file
                    ],
                    capture_output=True,
                    text=True,
                    timeout=10,
                    cwd=self.session_dir
                    )
                    
                    self.last_activity = time.time()
                    output = result.stdout.strip()
                    error_output = result.stderr.strip()
                    
                    if "TRUE" in output:
                        return True, "true"
                    elif "FALSE" in output:
                        return True, "false"
                    elif "Unknown procedure" in error_output:
                        # This means the predicate doesn't exist in this session - that's a false result
                        return True, "false"
                    else:
                        return False, f"Query error: {error_output}"
                        
                finally:
                    try:
                        os.unlink(script_file)
                    except:
                        pass
                        
            except subprocess.TimeoutExpired:
                return False, "Query timeout"
            except Exception as e:
                return False, f"Query error: {str(e)}"
    
    def add_clause(self, clause: str) -> Tuple[bool, str]:
        """Add clause to session"""
        if not clause.strip().endswith('.'):
            clause = clause.strip() + '.'
            
        with self.lock:
            self.clauses.append(clause)
            self.last_activity = time.time()
        
        return True, "Clause added"
    
    def remove_clause(self, clause: str) -> Tuple[bool, str]:
        """Remove clause from session"""
        if not clause.strip().endswith('.'):
            clause = clause.strip() + '.'
            
        with self.lock:
            if clause in self.clauses:
                self.clauses.remove(clause)
                self.last_activity = time.time()
                return True, "Clause removed"
            else:
                return False, "Clause not found"
    
    def get_clauses(self) -> list:
        """Get all clauses"""
        with self.lock:
            return self.clauses.copy()
    
    def query(self, query_str: str) -> Tuple[bool, str]:
        """Execute query"""
        return self.execute_query(query_str)
    
    def is_alive(self) -> bool:
        """Check if session is alive"""
        return os.path.exists(self.session_dir)
    
    def is_expired(self) -> bool:
        """Check if expired"""
        return (time.time() - self.last_activity) > self.timeout_sec


class IsolatedServer:
    """Multi-user server with process isolation"""

    def __init__(self, session_timeout_sec: int = 300):
        self.sessions: Dict[str, IsolatedSession] = {}
        self.session_timeout = session_timeout_sec
        self.cleanup_thread = None
        self.running = False
        self.lock = threading.Lock()
    
    def start(self):
        """Start server"""
        self.running = True
        self.cleanup_thread = threading.Thread(target=self._cleanup_expired_sessions, daemon=True)
        self.cleanup_thread.start()
    
    def stop(self):
        """Stop server"""
        self.running = False
        with self.lock:
            for session in list(self.sessions.values()):
                session.stop()
            self.sessions.clear()
    
    def create_session(self) -> str:
        """Create session"""
        session_id = str(uuid.uuid4())
        with self.lock:
            session = IsolatedSession(session_id, self.session_timeout)
            session.start()
            self.sessions[session_id] = session
        return session_id
    
    def destroy_session(self, session_id: str) -> bool:
        """Destroy session"""
        with self.lock:
            if session_id in self.sessions:
                self.sessions[session_id].stop()
                del self.sessions[session_id]
                return True
        return False
    
    def get_session(self, session_id: str) -> Optional[IsolatedSession]:
        """Get session"""
        with self.lock:
            return self.sessions.get(session_id)
    
    def add_clause(self, session_id: str, clause: str) -> Tuple[bool, str]:
        """Add clause"""
        session = self.get_session(session_id)
        if not session:
            return False, f"Session not found"
        return session.add_clause(clause)
    
    def remove_clause(self, session_id: str, clause: str) -> Tuple[bool, str]:
        """Remove clause"""
        session = self.get_session(session_id)
        if not session:
            return False, f"Session not found"
        return session.remove_clause(clause)
    
    def get_clauses(self, session_id: str) -> Optional[list]:
        """Get clauses"""
        session = self.get_session(session_id)
        if not session:
            return None
        return session.get_clauses()
    
    def query(self, session_id: str, query_str: str) -> Tuple[bool, str]:
        """Execute query"""
        session = self.get_session(session_id)
        if not session:
            return False, f"Session not found"
        return session.query(query_str)
    
    def list_sessions(self) -> Dict[str, dict]:
        """List sessions"""
        with self.lock:
            return {
                sid: {
                    'alive': session.is_alive(),
                    'last_activity': session.last_activity,
                    'clauses_count': len(session.clauses)
                }
                for sid, session in self.sessions.items()
            }
    
    def _cleanup_expired_sessions(self):
        """Background cleanup"""
        while self.running:
            try:
                with self.lock:
                    expired = [
                        sid for sid, session in self.sessions.items()
                        if session.is_expired() or not session.is_alive()
                    ]
                
                for session_id in expired:
                    self.destroy_session(session_id)
                
                time.sleep(30)
            except Exception as e:
                print(f"Cleanup error: {e}")
                time.sleep(30)
    
    def __enter__(self):
        self.start()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.stop()


# HTTP Server
from http.server import HTTPServer, BaseHTTPRequestHandler
import urllib.parse

class IsolatedHandler(BaseHTTPRequestHandler):
    server_instance = None
    
    def do_POST(self):
        path = self.path
        content_length = int(self.headers.get('Content-Length', 0))
        post_data = self.rfile.read(content_length).decode('utf-8')
        
        try:
            data = json.loads(post_data) if post_data else {}
        except json.JSONDecodeError:
            self._send_error(400, "Invalid JSON")
            return
        
        if path == '/session':
            self._handle_session(data)
        elif path == '/clause':
            self._handle_clause(data)
        elif path == '/query':
            self._handle_query(data)
        else:
            self._send_error(404, "Endpoint not found")
    
    def do_GET(self):
        parsed_path = urllib.parse.urlparse(self.path)
        path = parsed_path.path
        params = urllib.parse.parse_qs(parsed_path.query)
        
        if path == '/sessions':
            sessions = self.server_instance.list_sessions()
            self._send_json(sessions)
        elif path == '/clauses':
            session_id = params.get('session_id', [None])[0]
            if session_id:
                clauses = self.server_instance.get_clauses(session_id)
                if clauses is not None:
                    self._send_json({'clauses': clauses})
                else:
                    self._send_error(404, f"Session not found")
            else:
                self._send_error(400, "session_id required")
        else:
            self._send_error(404, "Endpoint not found")
    
    def _handle_session(self, data):
        action = data.get('action')
        if action == 'create':
            session_id = self.server_instance.create_session()
            self._send_json({'status': 'success', 'session_id': session_id})
        elif action == 'destroy':
            session_id = data.get('session_id')
            success = self.server_instance.destroy_session(session_id)
            if success:
                self._send_json({'status': 'success'})
            else:
                self._send_error(404, f"Session not found")
        else:
            self._send_error(400, "Invalid action")
    
    def _handle_clause(self, data):
        session_id = data.get('session_id')
        clause = data.get('clause')
        action = data.get('action', 'add')
        
        if not session_id or not clause:
            self._send_error(400, "session_id and clause required")
            return
        
        if action == 'add':
            success, output = self.server_instance.add_clause(session_id, clause)
        elif action == 'remove':
            success, output = self.server_instance.remove_clause(session_id, clause)
        else:
            self._send_error(400, "Invalid action")
            return
        
        if success:
            self._send_json({'status': 'success', 'output': output})
        else:
            self._send_error(500, f"Operation failed: {output}")
    
    def _handle_query(self, data):
        session_id = data.get('session_id')
        query = data.get('query')
        
        if not session_id or not query:
            self._send_error(400, "session_id and query required")
            return
        
        success, output = self.server_instance.query(session_id, query)
        
        if success:
            self._send_json({'status': 'success', 'result': output})
        else:
            self._send_error(500, f"Query failed: {output}")
    
    def _send_json(self, data):
        response = json.dumps(data)
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', len(response))
        self.end_headers()
        self.wfile.write(response.encode('utf-8'))
    
    def _send_error(self, code, message):
        error_response = json.dumps({'status': 'error', 'message': message})
        self.send_response(code)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', len(error_response))
        self.end_headers()
        self.wfile.write(error_response.encode('utf-8'))
    
    def log_message(self, format, *args):
        pass


def start_isolated_server(port: int = 8080):
    """Start isolated server"""
    server_instance = IsolatedServer()
    server_instance.start()

    IsolatedHandler.server_instance = server_instance

    httpd = HTTPServer(('localhost', port), IsolatedHandler)
    print(f"🔧 Isolated server started on http://localhost:{port}")
    
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\n🛑 Shutting down...")
    finally:
        server_instance.stop()
        httpd.server_close()


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description='Isolated Multi-User Server')
    parser.add_argument('--port', type=int, default=8080, help='Port (default: 8080)')
    args = parser.parse_args()

    start_isolated_server(args.port)