# MCP (Model Context Protocol) server that starts SWI Prolog server and allows LLMs to access it as a tool via MCP.

import argparse
from fastmcp import FastMCP
from Prolog_Server_start import PrologServerController

mcp = FastMCP(
    name="Prolog MCP Server",
    instructions=(
        "Used for LLMs to interact with a SWI Prolog server. "
        "There are separate tools to add and remove single Prolog clauses "
        "(clauses can be rules or facts), "
        "to list all currently active Prolog clauses, "
        "and to execute Prolog queries."
    )
)

# Create an instance of the PrologServerController
prolog = PrologServerController(port=9090)

# define single server functions as tools

@mcp.tool()
def add_clause(clause: str):
    """Add a Prolog clause to the server. A clause should solely exist of a single valid Prolog rule or Prolog fact, to be used for following Prolog queries."""
    return prolog.add_clause(clause)

@mcp.tool()
def get_clauses():
    """List all currently active Prolog clauses on the server."""
    return prolog.get_clauses()

@mcp.tool()
def remove_clause(clause: str):
    """Remove a Prolog clause from the server. The string must be identical to the one used in add_clause."""
    return prolog.remove_clause(clause)

@mcp.tool()
def query_prolog(query: str):
    """Execute a Prolog query on the server. The query must be a valid Prolog query string."""
    return prolog.query(query)

@mcp.tool()
def start_prolog_server():
    """Start the Prolog server."""
    try:
        prolog.start_server()
        return "Prolog server started."
    except Exception as e:
        return f"Failed to start Prolog server: {e}"

@mcp.tool()
def stop_prolog_server():
    """Stop the Prolog server."""
    try:
        prolog.stop_server()
        return "Prolog server stopped."
    except Exception as e:
        return f"Failed to stop Prolog server: {e}"


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Prolog MCP Server")
    parser.add_argument("--transport", choices=["stdio", "streamable-http"], 
                       default="streamable-http", 
                       help="Transport method (default: streamable-http)")
    args = parser.parse_args()
    
    print("Prolog Server instance should be running on port 9090")
    try:
        print(f"Starting MCP Server with {args.transport} transport...")
        mcp.run(transport=args.transport)
    except KeyboardInterrupt:
        print("MCP Server stopped by user.")
    finally:
        print("MCP Server stopped.")