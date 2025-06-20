#!/usr/bin/env python3
"""
Simple test client for the prolog-mcp-server package using stdio transport.
Uses the installed package directly.
"""

import asyncio
from fastmcp import Client
from fastmcp.client.transports import PythonStdioTransport

async def test_prolog_mcp():
    """Test the prolog MCP server functionality."""
    
    # Create stdio transport using the installed package
    transport = PythonStdioTransport("src/prolog_mcp_server.py", args=["--transport", "stdio"])
    
    print("🔗 Connecting to Prolog MCP server via stdio...")
    
    try:
        async with Client(transport) as session:
            print("✅ Connected successfully!")
            
            # List available tools
            print("\n📋 Available tools:")
            tools = await session.list_tools()
            for tool in tools:
                print(f"  - {tool.name}: {tool.description}")
            
            print("\n🧪 Testing Prolog server functionality...")
            
            # 1. Start the Prolog server
            print("\n1. Starting Prolog server...")
            result = await session.call_tool("start_prolog_server", {})
            print(f"   Result: {result[0].text}")
            
            # 2. Add some test clauses
            print("\n2. Adding test clauses...")
            
            # Add mother/father facts
            result = await session.call_tool("add_clause", {"clause": "mother(alice, bob)."})
            print(f"   Added mother clause: {result[0].text}")
            
            result = await session.call_tool("add_clause", {"clause": "father(bob, carol)."})
            print(f"   Added father clause: {result[0].text}")
            
            # Add grandmother rule
            result = await session.call_tool("add_clause", {"clause": "grandmother(GM, GC) :- mother(GM, P), parent(P, GC)."})
            print(f"   Added grandmother rule: {result[0].text}")
            
            result = await session.call_tool("add_clause", {"clause": "parent(X, Y) :- mother(X, Y)."})
            print(f"   Added parent rule: {result[0].text}")
            
            result = await session.call_tool("add_clause", {"clause": "parent(X, Y) :- father(X, Y)."})
            print(f"   Added parent rule: {result[0].text}")
            
            # 3. List current clauses
            print("\n3. Current clauses:")
            result = await session.call_tool("get_clauses", {})
            print(f"   {result[0].text}")
            
            # 4. Test grandmother query
            print("\n4. Testing grandmother query...")
            result = await session.call_tool("query_prolog", {"query": "grandmother(GM, carol)."})
            print(f"   Query result: {result[0].text}")
            
            # 5. Test a simple membership query
            print("\n5. Testing membership query...")
            result = await session.call_tool("query_prolog", {"query": "member(2, [1,2,3])."})
            print(f"   Query result: {result[0].text}")
            
            # 6. Remove a clause
            print("\n6. Removing a clause...")
            result = await session.call_tool("remove_clause", {"clause": "father(bob, carol)."})
            print(f"   Remove result: {result[0].text}")
            
            # 7. List clauses after removal
            print("\n7. Clauses after removal:")
            result = await session.call_tool("get_clauses", {})
            print(f"   {result[0].text}")
            
            # 8. Stop the server
            print("\n8. Stopping Prolog server...")
            result = await session.call_tool("stop_prolog_server", {})
            print(f"   Result: {result[0].text}")
            
            print("\n✅ Test completed successfully!")
            
    except Exception as e:
        print(f"❌ Error: {e}")
        import traceback
        traceback.print_exc()

if __name__ == "__main__":
    asyncio.run(test_prolog_mcp())