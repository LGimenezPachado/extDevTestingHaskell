FROM mcr.microsoft.com/devcontainers/base:debian

# Instalar dependencias
RUN apt-get update && apt-get install -y curl build-essential libffi-dev libgmp-dev

# Cambiar a usuario vscode
USER vscode

# Establecer HOME explícitamente
ENV HOME=/home/vscode
ENV PATH="$HOME/.ghcup/bin:$PATH"

# Instalar ghcup y herramientas
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | bash -s -- --yes && \
    echo 'source $HOME/.ghcup/env' >> $HOME/.bashrc && \
    . $HOME/.ghcup/env && \
    ghcup install ghc && \
    ghcup set ghc && \
    ghcup install stack && \
    ghcup set stack
