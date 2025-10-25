#!/usr/bin/env python3
"""
Multi-user Prolog server using separate SWI-Prolog processes for isolation.
Each user session gets its own isolated SWI-Prolog process.
"""

import subprocess
import threading
import json
import time
import uuid
import os
import signal
from typing import Dict, Optional, Tuple
from contextlib import contextmanager
import queue
import tempfile

class PrologSession:
    """Manages a single user's isolated Prolog process"""
    
    def __init__(self, session_id: str, timeout_sec: int = 300):
        self.session_id = session_id
        self.timeout_sec = timeout_sec
        self.process: Optional[subprocess.Popen] = None
        self.last_activity = time.time()
        self.lock = threading.Lock()
        self.clauses = []  # Track clauses for this session
        self.session_dir = None  # Isolated working directory for this session
        
    def start(self):
        """Initialize the session with isolated working directory"""
        with self.lock:
            # Create isolated session directory
            self.session_dir = tempfile.mkdtemp(prefix=f"prolog_session_{self.session_id[:8]}_")
            self.temp_file = tempfile.NamedTemporaryFile(mode='w', suffix='.pl', delete=False, dir=self.session_dir)
            self.temp_file.write("% Session placeholder")  
            self.temp_file.close()
            self.last_activity = time.time()
    
    def stop(self):
        """Clean up session resources"""
        with self.lock:
            # Cleanup session directory and all files
            if self.session_dir and os.path.exists(self.session_dir):
                try:
                    import shutil
                    shutil.rmtree(self.session_dir)
                except:
                    pass
    
    def execute_prolog(self, prolog_code: str) -> Tuple[bool, str]:
        """Execute Prolog code using a fresh SWI-Prolog process in isolated directory"""
        with self.lock:
            if not self.session_dir or not os.path.exists(self.session_dir):
                return False, "Session not initialized"
            
            try:
                # Create a completely isolated script in the session directory
                script_content = f"""
% Isolated Prolog session - no shared state
:- abolish_all_tables.

% Define all predicates as dynamic to allow runtime modification
{chr(10).join([f":- dynamic({self._extract_predicate_name(clause)})." for clause in set([self._extract_predicate_name(c) for c in self.clauses])])}

% Load ONLY this session's clauses
{chr(10).join([f"assertz({clause.rstrip('.')})." for clause in self.clauses])}

% Execute the query with proper success/failure detection
test_query :-
    {prolog_code.rstrip('.')}, 
    write('true'), nl, !.
test_query :-
    write('false'), nl.

% Execute and halt
:- test_query, halt.
"""
                
                # Write to session-specific file  
                query_file_path = os.path.join(self.session_dir, f"query_{int(time.time() * 1000)}.pl")
                with open(query_file_path, 'w') as f:
                    f.write(script_content)
                
                # Debug: print the generated script for troubleshooting
                if os.getenv('DEBUG_PROLOG'):
                    print(f"=== Generated Script for {self.session_id[:8]} ===")
                    print(script_content)
                    print("=" * 50)
                
                try:
                    # Run SWI-Prolog in the isolated session directory
                    result = subprocess.run([
                        'swipl',
                        '-q',  # Quiet
                        '--no-packs',  # Don't load global packs
                        '--no-debug',  # No debugging
                        query_file_path
                    ], 
                    capture_output=True, 
                    text=True, 
                    timeout=10,
                    cwd=self.session_dir  # Run in isolated directory
                    )
                    
                    self.last_activity = time.time()
                    
                    output = result.stdout.strip()
                    error_output = result.stderr.strip()
                    
                    if result.returncode == 0:
                        # Check the actual Prolog output, not just process success
                        if "true" in output:
                            return True, "true"
                        elif "false" in output:
                            return True, "false"  # Query ran successfully but returned false
                        else:
                            return True, output if output else "true"
                    else:
                        return False, error_output if error_output else "Query failed"
                        
                finally:
                    # Cleanup temp query file
                    try:
                        os.unlink(query_file_path)
                    except:
                        pass
                
            except subprocess.TimeoutExpired:
                return False, "Query timeout"
            except Exception as e:
                return False, f"Execution error: {str(e)}"
    
    def add_clause(self, clause: str) -> Tuple[bool, str]:
        """Add a clause to this session"""
        # Ensure clause ends with period
        if not clause.strip().endswith('.'):
            clause = clause.strip() + '.'
        
        # Simply store the clause - don't execute it yet
        with self.lock:
            self.clauses.append(clause)
            self.last_activity = time.time()
        
        return True, "Clause added"
    
    def remove_clause(self, clause: str) -> Tuple[bool, str]:
        """Remove a clause from this session"""
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
        """Get all clauses for this session"""
        return self.clauses.copy()
    
    def query(self, query_str: str) -> Tuple[bool, str]:
        """Execute a Prolog query"""
        if not query_str.strip().endswith('.'):
            query_str = query_str.strip() + '.'
        return self.execute_prolog(query_str)
    
    def _extract_predicate_name(self, clause: str) -> str:
        """Extract predicate name/arity from a clause for dynamic declaration"""
        clause = clause.strip().rstrip('.')
        
        # Handle facts like: likes(mary, food)
        if '(' in clause:
            predicate_part = clause.split('(')[0].strip()
            # Count parameters by finding matching parentheses
            paren_content = clause[clause.find('(') + 1:clause.rfind(')')].strip()
            if paren_content:
                # Simple parameter counting (not perfect for complex terms but good enough)
                param_count = len([p.strip() for p in paren_content.split(',') if p.strip()])
            else:
                param_count = 0
            return f"{predicate_part}/{param_count}"
        else:
            # Handle facts without parameters
            return f"{clause}/0"
    
    def is_alive(self) -> bool:
        """Check if the session is still active"""
        with self.lock:
            return self.session_dir and os.path.exists(self.session_dir)
    
    def is_expired(self) -> bool:
        """Check if session has expired due to inactivity"""
        return (time.time() - self.last_activity) > self.timeout_sec


class MultiUserPrologServer:
    """Multi-user Prolog server with process-based isolation"""
    
    def __init__(self, session_timeout_sec: int = 300):
        self.sessions: Dict[str, PrologSession] = {}
        self.session_timeout = session_timeout_sec
        self.cleanup_thread = None
        self.running = False
        self.lock = threading.Lock()
    
    def start(self):
        """Start the server and cleanup thread"""
        self.running = True
        self.cleanup_thread = threading.Thread(target=self._cleanup_expired_sessions, daemon=True)
        self.cleanup_thread.start()
    
    def stop(self):
        """Stop the server and cleanup all sessions"""
        self.running = False
        
        with self.lock:
            for session in list(self.sessions.values()):
                session.stop()
            self.sessions.clear()
    
    def create_session(self) -> str:
        """Create a new user session"""
        session_id = str(uuid.uuid4())
        
        with self.lock:
            session = PrologSession(session_id, self.session_timeout)
            session.start()
            self.sessions[session_id] = session
        
        return session_id
    
    def destroy_session(self, session_id: str) -> bool:
        """Destroy a user session"""
        with self.lock:
            if session_id in self.sessions:
                self.sessions[session_id].stop()
                del self.sessions[session_id]
                return True
        return False
    
    def get_session(self, session_id: str) -> Optional[PrologSession]:
        """Get a session by ID"""
        with self.lock:
            return self.sessions.get(session_id)
    
    def add_clause(self, session_id: str, clause: str) -> Tuple[bool, str]:
        """Add clause to a specific session"""
        session = self.get_session(session_id)
        if not session:
            return False, f"Session {session_id} not found"
        return session.add_clause(clause)
    
    def remove_clause(self, session_id: str, clause: str) -> Tuple[bool, str]:
        """Remove clause from a specific session"""
        session = self.get_session(session_id)
        if not session:
            return False, f"Session {session_id} not found"
        return session.remove_clause(clause)
    
    def get_clauses(self, session_id: str) -> Optional[list]:
        """Get all clauses for a specific session"""
        session = self.get_session(session_id)
        if not session:
            return None
        return session.get_clauses()
    
    def query(self, session_id: str, query_str: str) -> Tuple[bool, str]:
        """Execute query in a specific session"""
        session = self.get_session(session_id)
        if not session:
            return False, f"Session {session_id} not found"
        return session.query(query_str)
    
    def list_sessions(self) -> Dict[str, dict]:
        """List all active sessions"""
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
        """Background thread to cleanup expired sessions"""
        while self.running:
            try:
                with self.lock:
                    expired_sessions = [
                        sid for sid, session in self.sessions.items()
                        if session.is_expired() or not session.is_alive()
                    ]
                
                for session_id in expired_sessions:
                    print(f"Cleaning up expired session: {session_id[:8]}...")
                    self.destroy_session(session_id)
                
                time.sleep(30)  # Check every 30 seconds
            except Exception as e:
                print(f"Cleanup thread error: {e}")
                time.sleep(30)
    
    def __enter__(self):
        self.start()
        return self
    
    def __exit__(self, exc_type, exc_val, exc_tb):
        self.stop()


# Simple HTTP server interface
from http.server import HTTPServer, BaseHTTPRequestHandler
import urllib.parse

class MultiUserPrologHandler(BaseHTTPRequestHandler):
    server_instance = None
    
    def do_POST(self):
        """Handle POST requests"""
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
        """Handle GET requests"""
        parsed_path = urllib.parse.urlparse(self.path)
        path = parsed_path.path
        params = urllib.parse.parse_qs(parsed_path.query)
        
        if path == '/sessions':
            self._handle_list_sessions()
        elif path == '/clauses':
            session_id = params.get('session_id', [None])[0]
            if session_id:
                self._handle_get_clauses(session_id)
            else:
                self._send_error(400, "session_id parameter required")
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
                self._send_error(404, f"Session {session_id} not found")
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
    
    def _handle_list_sessions(self):
        sessions = self.server_instance.list_sessions()
        self._send_json(sessions)
    
    def _handle_get_clauses(self, session_id):
        clauses = self.server_instance.get_clauses(session_id)
        if clauses is not None:
            self._send_json({'clauses': clauses})
        else:
            self._send_error(404, f"Session {session_id} not found")
    
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
        # Suppress default logging
        pass


def start_multi_user_server(port: int = 8080):
    """Start the multi-user Prolog server"""
    server_instance = MultiUserPrologServer()
    server_instance.start()
    
    # Set the server instance for the handler
    MultiUserPrologHandler.server_instance = server_instance
    
    # Start HTTP server
    httpd = HTTPServer(('localhost', port), MultiUserPrologHandler)
    print(f"🚀 Multi-user Prolog server started on http://localhost:{port}")
    
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        print("\n🛑 Shutting down server...")
    finally:
        server_instance.stop()
        httpd.server_close()


if __name__ == "__main__":
    import argparse
    
    parser = argparse.ArgumentParser(description='Multi-User Prolog Server')
    parser.add_argument('--port', type=int, default=8080, help='Server port (default: 8080)')
    args = parser.parse_args()
    
    start_multi_user_server(args.port)