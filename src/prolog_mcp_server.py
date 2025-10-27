# MCP (Model Context Protocol) server that starts SWI Prolog server and allows LLMs to access it as a tool via MCP.

import argparse
import threading
from fastmcp import FastMCP
from prolog_server_start import PrologServerController
from pengine_controller import PengineController
from multi_user_controller import MultiUserPrologController
from isolated_controller import IsolatedController

# Global variables for different backends
prolog = None
pengine_controller = None
multi_user_controller = None
isolated_controller = None
current_session_id = None
session_lock = threading.Lock()

def initialize_backend(transport="streamable-http", backend="isolated"):
    """Initialize the appropriate backend based on transport and backend type."""
    global prolog, pengine_controller, multi_user_controller, isolated_controller
    
    if transport == "stdio":
        # Single-user mode with Docker backend
        prolog = PrologServerController(port=9090)
        print("Using single-user Docker backend for stdio transport")
    else:
        # Multi-user mode - choose backend
        if backend == "pengines":
            pengine_controller = PengineController(port=8080)
            print("Using Pengines backend for HTTP transport (experimental)")
        elif backend == "multi-user":
            multi_user_controller = MultiUserPrologController(port=8081)
            print("Using new multi-user backend for HTTP transport")
        elif backend == "isolated":
            isolated_controller = IsolatedController(port=8082)
            print("Using isolated backend for HTTP transport (RECOMMENDED)")
        else:
            prolog = PrologServerController(port=9090)
            print("Using Docker backend for HTTP transport")

# Custom FastMCP subclass that handles backend initialization
class PrologMCP(FastMCP):
    def __init__(self, name, instructions):
        super().__init__(name=name, instructions=instructions)
        self._backend_initialized = False
    
    def run(self, transport="streamable-http", backend="isolated", **kwargs):
        # Initialize backend just before running if not already done
        if not self._backend_initialized:
            initialize_backend(transport, backend)
            self._backend_initialized = True
        
        # Call parent run method
        super().run(transport=transport, **kwargs)

# Create MCP server instance
mcp = PrologMCP(
    name="Prolog MCP Server",
    instructions=(
        "Used for LLMs to interact with a SWI Prolog server. "
        "Supports single-user (stdio) and multi-user (HTTP) modes with session isolation. "
        "Each user gets an separate Prolog prcess. "
        "There are separate tools to add and remove single Prolog clauses "
        "(clauses can be rules or facts), "
        "to list all currently active Prolog clauses, "
        "and to execute Prolog queries."
    )
)

# define single server functions as tools

@mcp.tool()
def add_clause(clause: str):
    """Add a Prolog clause to the server. A clause should solely exist of a single valid Prolog rule or Prolog fact, to be used for following Prolog queries."""
    if prolog:  # Single-user mode
        return prolog.add_clause(clause)
    elif pengine_controller and current_session_id:  # Pengine multi-user mode
        return pengine_controller.add_clause(current_session_id, clause)
    elif multi_user_controller and current_session_id:  # New multi-user mode
        return multi_user_controller.add_clause(current_session_id, clause)
    elif isolated_controller and current_session_id:  #  Isolated mode
        return isolated_controller.add_clause(current_session_id, clause)
    else:
        return "Error: No Prolog backend available"

@mcp.tool()
def get_clauses():
    """List all currently active Prolog clauses on the server."""
    if prolog:  # Single-user mode
        return prolog.get_clauses()
    elif pengine_controller and current_session_id:  # Pengine multi-user mode
        return pengine_controller.get_clauses(current_session_id)
    elif multi_user_controller and current_session_id:  # New multi-user mode
        return multi_user_controller.get_clauses(current_session_id)
    elif isolated_controller and current_session_id:  # Isolated mode
        return isolated_controller.get_clauses(current_session_id)
    else:
        return "Error: No Prolog backend available"

@mcp.tool()
def remove_clause(clause: str):
    """Remove a Prolog clause from the server. The string must be identical to the one used in add_clause."""
    if prolog:  # Single-user mode
        return prolog.remove_clause(clause)
    elif pengine_controller and current_session_id:  # Pengine multi-user mode
        return pengine_controller.remove_clause(current_session_id, clause)
    elif multi_user_controller and current_session_id:  # New multi-user mode
        return multi_user_controller.remove_clause(current_session_id, clause)
    elif isolated_controller and current_session_id:  # Isolated mode
        return isolated_controller.remove_clause(current_session_id, clause)
    else:
        return "Error: No Prolog backend available"

@mcp.tool()
def query_prolog(query: str):
    """Execute a Prolog query on the server. The query must be a valid Prolog query string."""
    if prolog:  # Single-user mode
        return prolog.query(query)
    elif pengine_controller and current_session_id:  # Pengine multi-user mode
        return pengine_controller.query(current_session_id, query)
    elif multi_user_controller and current_session_id:  # New multi-user mode
        return multi_user_controller.query(current_session_id, query)
    elif isolated_controller and current_session_id:  # Isolated mode
        return isolated_controller.query(current_session_id, query)
    else:
        return "Error: No Prolog backend available"

@mcp.tool()
def start_prolog_server():
    """Start the Prolog server."""
    global current_session_id
    try:
        if prolog:  # Single-user mode
            prolog.start_server()
            return "Prolog server started."
        elif pengine_controller:  # Pengine multi-user mode
            pengine_controller.start_server()
            with session_lock:
                current_session_id = pengine_controller.create_session()
            return f"Pengine server started. Session ID: {current_session_id[:8]}..."
        elif multi_user_controller:  # New multi-user mode
            multi_user_controller.start_server()
            with session_lock:
                current_session_id = multi_user_controller.create_session()
            return f"Multi-user server started. Session ID: {current_session_id[:8]}..."
        elif isolated_controller:  # Isolated mode
            isolated_controller.start_server()
            with session_lock:
                current_session_id = isolated_controller.create_session()
            return f"Isolated server started. Session ID: {current_session_id[:8]}..."
        else:
            return "Error: No Prolog backend configured"
    except Exception as e:
        return f"Failed to start Prolog server: {e}"

@mcp.tool()
def stop_prolog_server():
    """Stop the Prolog server."""
    global current_session_id
    try:
        if prolog:  # Single-user mode
            prolog.stop_server()
            return "Prolog server stopped."
        elif pengine_controller:  # Pengine multi-user mode
            if current_session_id:
                pengine_controller.destroy_session(current_session_id)
                current_session_id = None
            pengine_controller.stop_server()
            return "Pengine server stopped."
        elif multi_user_controller:  # New multi-user mode
            if current_session_id:
                multi_user_controller.destroy_session(current_session_id)
                current_session_id = None
            multi_user_controller.stop_server()
            return "Multi-user server stopped."
        elif isolated_controller:  # Isolated mode
            if current_session_id:
                isolated_controller.destroy_session(current_session_id)
                current_session_id = None
            isolated_controller.stop_server()
            return "Isolated server stopped."
        else:
            return "Error: No Prolog backend configured"
    except Exception as e:
        return f"Failed to stop Prolog server: {e}"


@mcp.tool()
def create_user_session():
    """Create a new user session (multi-user mode only)."""
    if pengine_controller:
        try:
            session_id = pengine_controller.create_session()
            return f"Created session: {session_id[:8]}..."
        except Exception as e:
            return f"Failed to create session: {e}"
    elif multi_user_controller:
        try:
            session_id = multi_user_controller.create_session()
            return f"Created session: {session_id[:8]}..."
        except Exception as e:
            return f"Failed to create session: {e}"
    elif isolated_controller:
        try:
            session_id = isolated_controller.create_session()
            return f"Created session: {session_id[:8]}..."
        except Exception as e:
            return f"Failed to create session: {e}"
    else:
        return "Error: Multi-user mode not available"

@mcp.tool()
def destroy_user_session(session_id: str):
    """Destroy a user session (multi-user mode only)."""
    if pengine_controller:
        try:
            pengine_controller.destroy_session(session_id)
            return f"Destroyed session: {session_id[:8]}..."
        except Exception as e:
            return f"Failed to destroy session: {e}"
    elif multi_user_controller:
        try:
            multi_user_controller.destroy_session(session_id)
            return f"Destroyed session: {session_id[:8]}..."
        except Exception as e:
            return f"Failed to destroy session: {e}"
    elif isolated_controller:
        try:
            isolated_controller.destroy_session(session_id)
            return f"Destroyed session: {session_id[:8]}..."
        except Exception as e:
            return f"Failed to destroy session: {e}"
    else:
        return "Error: Multi-user mode not available"

def main():
    """Main entry point for the prolog-mcp-server command."""
    parser = argparse.ArgumentParser(description="Prolog MCP Server")
    parser.add_argument("--transport", choices=["stdio", "streamable-http"], 
                       default="streamable-http", 
                       help="Transport method (default: streamable-http)")
    parser.add_argument("--backend", choices=["docker", "pengines", "multi-user", "isolated"],
                       default="isolated",
                       help="Backend type (default: isolated)")
    args = parser.parse_args()
    
    # Re-initialize backend with command line arguments
    initialize_backend(args.transport, args.backend)
    
    try:
        print(f"Starting MCP Server with {args.transport} transport...")
        mcp.run(transport=args.transport)
    except KeyboardInterrupt:
        print("MCP Server stopped by user.")
    finally:
        # Cleanup
        if prolog:
            try:
                prolog.stop_server()
            except:
                pass
        if pengine_controller:
            try:
                pengine_controller.stop_server()
            except:
                pass
        if multi_user_controller:
            try:
                multi_user_controller.stop_server()
            except:
                pass
        if isolated_controller:
            try:
                isolated_controller.stop_server()
            except:
                pass
        print("MCP Server stopped.")


if __name__ == "__main__":
    main()
