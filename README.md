# Prolog MCP Server

A Model Context Protocol (MCP) server that enables seamless integration between Large Language Models and SWI-Prolog. This server allows LLMs to execute Prolog queries and leverage logical reasoning capabilities.

## Features

- Execute Prolog queries through MCP protocol
- Support for both HTTP and stdio transport
- Docker containerization support
- Easy integration with LLM applications


## Prerequisites

- Python 3.8 or higher
- SWI-Prolog installed on your system
- Git (for installation from source)

## Installation

1. Get source code from github

```bash
git clone https://github.com/wendelinism/prolog-mcp-server.git
```

2. Install python package from source code

Move into the downloaded folder and install:

```bash
cd ./prolog-mcp-server/
pip install -e .
```

## Usage

### Starting the Server

For HTTP transport setup, see the [HTTP transport implementation](examples/demo_prolog_MCP-server-start_http.py#L11-L22).

### Examples

Check the `examples/` directory for:
- HTTP transport demo
- Stdio transport demo  
- Client implementation examples

## Docker Support

A Dockerfile is provided in the `docker/` directory for containerized deployment.

## Credits

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## Credits

This MCP server relies on [SWI-Prolog](https://www.swi-prolog.org/), an open-source Prolog implementation. All credits for the actual Prolog implementation go to the SWI-Prolog development team.

