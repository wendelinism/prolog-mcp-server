import requests
import docker
import time

from docker.errors import DockerException, APIError, ImageNotFound

class PrologServerController:
    def __package_clause(self, clause_text):
        """Format clause for Prolog interpretation"""
        clause = clause_text.strip()
        if not clause.endswith('.'):
            clause += '.'  # Ensure the clause ends with a period
        return clause

    def __init__(self, port=8080, image_name="prolog-docker-image"):
        """
        Args:
            port: Host and container port (must match)
            image_name: Your Prolog Docker image name
        """
        self.port = port
        self.image_name = image_name
        self.container = None  
        self.clause_cache = set()  # Tracks active clauses

        try:
            self.docker_client = docker.from_env()
        except DockerException as e:
            raise RuntimeError(
                f"Docker is not available: {e}\n"
                f"Please ensure Docker is installed and running.\n"
                f"See installation instructions in README.md"
            )

    def start_server(self, timeout_sec=20):
        """Start the Prolog server container"""
        if self.container:
            raise RuntimeError("Server already running")
        
        try:
            self.container = self.docker_client.containers.run(
                self.image_name,
                ports={f"{self.port}/tcp": self.port},
                environment={"PORT": str(self.port)},
                detach=True,
                remove=True  # Auto-remove when stopped
            )
            print(f"⏳ Starting Prolog server on port {self.port}...")
            
            # Wait for server readiness
            start_time = time.time()
            while (time.time() - start_time) < timeout_sec:
                try:
                    if requests.get(
                        f"http://localhost:{self.port}/hello",
                        timeout=1
                    ).status_code == 200:
                        print(f"✅ Server ready at http://localhost:{self.port}")
                        return True
                except (requests.ConnectionError, requests.Timeout):
                    time.sleep(0.5)
            
            raise TimeoutError(f"Server didn't start within {timeout_sec} seconds")
            
        except ImageNotFound:
            raise RuntimeError(
                f"Docker image '{self.image_name}' not found.\n"
                f"Please build the Prolog docker image first:\n"
                f"  docker buildx build -t {self.image_name} -f docker/prolog.dockerfile .\n"
                f"See installation instructions in README.md for details."
            )
        except APIError as e:
            raise RuntimeError(
                f"Failed to start Docker container: {e}\n"
                f"Please ensure Docker daemon is running and you have sufficient permissions."
            )

    def stop_server(self):
        """Stop and clean up the container"""
        if self.container:
            try:
                self.container.stop(timeout=5)
                print("🛑 Server stopped")
            except APIError as e:
                raise RuntimeError(f"Failed to stop container: {e}")
            finally:
                self.container = None

    def add_clause(self, clause_text):
        """
        Add a clause to the knowledge base
        Args:
            clause_text: Prolog clause (e.g. "father(john,mary).")
        """
        clause = self.__package_clause(clause_text)
        try:            
            response = requests.post(
                f"http://localhost:{self.port}/assert",
                json={"clause": clause},
                timeout=5
            )
            response.raise_for_status()
            self.clause_cache.add(clause)
            print(f"➕ Added clause: {clause}")
            return True
        except requests.RequestException as e:
            raise ConnectionError(f"Clause addition failed: {e}")

    def remove_clause(self, clause_text):
        """
        Remove a clause from the knowledge base
        Args:
            clause_text: Prolog clause to retract (e.g. "father(john,mary).")
        """
        clause = self.__package_clause(clause_text)
        try:
            response = requests.post(
                f"http://localhost:{self.port}/retract",
                json={"clause": clause},
                timeout=5
            )
            response.raise_for_status()
            self.clause_cache.discard(clause)
            print(f"➖ Removed clause: {clause}")
            return True
        except requests.RequestException as e:
            raise ConnectionError(f"Clause removal failed: {e}")

    def get_clauses(self):
        """List all currently active clauses"""
        try:
            response = requests.get(
                f"http://localhost:{self.port}/list_clauses",
                timeout=5
            )
            response.raise_for_status()
            return response.json()
        except requests.RequestException as e:
            raise ConnectionError(f"Failed to fetch clauses: {e}")
        
    def query(self, prolog_query, timeout_sec=5):
        """Execute a Prolog query"""
        try:
            response = requests.get(
                f"http://localhost:{self.port}/query",
                params={"q": prolog_query},
                timeout=timeout_sec
            )
            response.raise_for_status()
            return response.json()
        except requests.RequestException as e:
            raise ConnectionError(f"Query failed: {e}")

    def __enter__(self):
        """For context manager usage"""
        self.start_server()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Ensure cleanup"""
        self.stop_server()

if __name__ == "__main__":
    print("This module provides PrologServerController class.")
    print("See examples/demo.py for usage examples.")