# Prolog MCP Server

A Model Context Protocol (MCP) server that enables direct integration between Large Language Models and SWI-Prolog. This server allows LLMs to define Prolog facts and rules (clauses) and to execute Prolog queries and leverage logical reasoning capabilities.



## Features

- Allow LLMs to add facts and rules (clauses) to SWI Prolog server
- Execute Prolog queries through MCP protocol
- Support for both HTTP and stdio transport
- Docker containerization support for the SWI Prolog server
- Implemented as python package for easy integration to agentic LLM applications



## Prerequisites

- Python 3.8 or higher
- SWI-Prolog installed on your system
- Git (for installation from source)

## Installation on linux

1. Get source code from github
Install git 
```bash
sudo apt install git
```

```bash
git clone https://github.com/wendelinism/prolog-mcp-server.git
```
2. Prepare python environment
Install python venv and pip:
```bash
sudo apt install python3.12-venv
```
Move into newly downloaded folder
```bash
cd prolog-mcp-server/
```
Create local python environment
```bash
python3 -m venv .
```


3. Install python package from source code

Activate local python environment
```bash
source bin/activate
```

Install python package prolog-mcp-server
```bash
pip install -e .
```

4. Install docker and build prolog docker image
```bash
sudo apt install docker-buildx
```
Add local user do docker group:
```bash
sudo usermod -aG docker $USER
```
Need to lock out and back in to activate group change.
Build docker image:
```bash
docker buildx build -t prolog-docker-image -f docker/prolog.dockerfile .
```

## Usage

### Starting the Server

For HTTP transport setup, see the [HTTP transport implementation](examples/demo_prolog_MCP-server-start_http.py#L11-L22).

### Examples

Check the `examples/` directory for:
- HTTP transport demo
- Stdio transport demo  
- Client implementation examples

To start standalon Prolog MCP server with http server:
```bash
python3 examples/demo_prolog_MCP-server-start_http.py
```

Then, to test running MCP server with MCP inspector, first install npm:
```bash
sudo apt install npm -y
```
When MCP server is running with http transport, run in separate terminal:
```bash
fastmcp dev examples/demo_prolog_MCP-server-start_http.py
```

Follow link to MCP inspector with prefilled token.
In the web interface make sure to adjust transport type ("Streamable HTTP") and URL ("http://localhost:8000/mcp")
Hit connect, for test you should be able to go to "tools" now, and there "list tools"

## Docker Support

A Dockerfile is provided in the `docker/` directory for containerized deployment.

## Credits

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## Credits

This MCP server relies on [SWI-Prolog](https://www.swi-prolog.org/), an open-source Prolog implementation. All credits for the actual Prolog implementation go to the SWI-Prolog development team.

