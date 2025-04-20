# Use the opam base image
FROM ocaml/opam:latest

# Set the working directory inside the container
WORKDIR /home/opam/app

# Install required packages
RUN sudo apt-get update && sudo apt-get install -y npm python3 libgmp-dev pkg-config libffi-dev libssl-dev zlib1g-dev 

# Install dune using opam
RUN opam update && opam switch create 5.2.0 && eval $(opam env) && opam install -y dune

# Copy the current directory into the container
COPY --chown=opam:opam . .
# Run make deps to install dependencies
# RUN opam switch 5.2.0 && make deps
# Run make test to execute tests
CMD ["make", "test"]
