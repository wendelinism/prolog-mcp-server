#!/usr/bin/env python3
"""
Minimal Prolog MCP server start using http for transport. 
Uses the installed package prolog-mcp-server by importing it directly.
The server can then be accessed either via MCP inspector or via any MCP client that supports HTTP transport.
For demo of direct start of the MCP server from a client using STDIO (instead of http) transport, see separate demo script.
"""

import prolog_mcp_server

# Expose the server object for fastmcp dev to run MCP inspector, must use a standard variable name (mcp, server, or app)   
mcp = prolog_mcp_server.prolog_mcp

if __name__ == "__main__":
    print("🚀 Starting Prolog MCP Server with HTTP transport...")
    print("📡 Server will be available at: http://127.0.0.1:8000/mcp")
    print("🛑 Press Ctrl+C to stop the server")
    print("-" * 50)
    
    try:
        # This will run the server
        mcp.run(transport="streamable-http")
    except KeyboardInterrupt:
        print("\n🛑 Server stopped by user")