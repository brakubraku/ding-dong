FROM ubuntu:24.04

# Install dependencies
RUN apt-get update && apt-get install -y \
    curl git build-essential pkg-config sudo xz-utils

# Download Nix installer (cached separately)
RUN curl -L https://nixos.org/nix/install -o /tmp/nix-install.sh && chmod +x /tmp/nix-install.sh

# Install Nix as root
RUN /tmp/nix-install.sh --daemon

# Switch to bash shell for Nix commands
SHELL ["/bin/bash", "-c"]

ENV USER=root

# Source Nix profile and update channel
RUN source /root/.nix-profile/etc/profile.d/nix.sh && \
    nix-channel --update
    
# Enable flakes
RUN mkdir -p /root/.config/nix && \
    echo "experimental-features = nix-command flakes" >> /root/.config/nix/nix.conf

RUN apt-get install build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config -y

ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
ENV BOOTSTRAP_HASKELL_MINIMAL=1

# Install ghcup 
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh 

# Install GHC and HLS using ghcup 
RUN source /root/.ghcup/env && ghcup install ghc 9.10.3
RUN source /root/.ghcup/env && ghcup set ghc 9.10.3
RUN source /root/.ghcup/env && ghcup install hls
RUN source /root/.ghcup/env && ghcup install cabal && cabal update

# Add ghcup environment to bashrc
RUN echo "source /root/.ghcup/env" >> /root/.bashrc

# Install libsecp256k1-dev for cryptographic operations
RUN apt-get install libsecp256k1-dev libz-dev -y

CMD ["bash"]