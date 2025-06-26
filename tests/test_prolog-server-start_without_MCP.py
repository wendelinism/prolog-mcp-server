#!/usr/bin/env python3
"""
Demo script showing how to use the PrologServerController
"""

import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..', 'src'))

from prolog_server_start import PrologServerController

if __name__ == "__main__":
    try:
        with PrologServerController(port=8080) as server:
            print(server.query("member(X,[1,2,3])"))
            # Clause management tests
            print("Adding clauses...")
            server.add_clause("uncle(Uncle, Child) :- brother(Uncle, Parent), parent(Parent, Child).")
            server.add_clause("brother(heinrich, thomas).")
            server.add_clause("parent(thomas, monika).")
            print("Current clauses:", server.get_clauses())
            
            # Execute a query, should return, true, as all clauses are present to satisfy the query
            print("Family query: ",server.query("uncle(heinrich,monika)"))
            print("Removing clauses...")
            server.remove_clause("parent(thomas, monika).")
            print("Current clauses after removal:", server.get_clauses())
            # Execute the query again, should return false, as one clause has been removed, can't confirm aht heinrich is monika's uncle
            print("Family query: ",server.query("uncle(heinrich,monika)"))          
            
    except Exception as e:
        print(f"❌ Full error: {str(e)}")
        if hasattr(e, 'response'):
            print("Server response:", e.response.text)