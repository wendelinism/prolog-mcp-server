FROM swipl:latest

LABEL description="Prolog MCP Server"

WORKDIR /app

# Verify HTTP libraries are available
RUN swipl -g "use_module(library(http/http_server)), halt(0)" && \
    swipl -g "use_module(library(http/http_json)), halt(0)"

# Copy server file
COPY server.pl .

# Set default port
ENV PORT=8080

# Start server
CMD ["swipl", "-s", "server.pl"]