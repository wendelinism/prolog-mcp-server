"""
Prolog MCP Server - Model Context Protocol Server for SWI Prolog integration
"""

from .prolog_server_start import PrologServerController
from .prolog_mcp_server import prolog_mcp

__version__ = "0.1.0"
__all__ = ["PrologServerController", "prolog_mcp", "__version__"]