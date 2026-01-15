FROM public.ecr.aws/lts/ubuntu:22.04_stable

ARG DEBIAN_FRONTEND=noninteractive
ARG KEYRING_PATH=/usr/share/keyrings
ARG APT_SOURCES_PATH=/etc/apt/sources.list.d

# update and upgrade
RUN apt update && apt upgrade -y

# install essentialls
RUN apt update && \
    apt install -y \
    man make build-essential git zsh vim curl wget procps gnupg gnupg2 ca-certificates zip \
    software-properties-common

# unminimize the system
RUN bash -c "yes | unminimize"

# create dev sudo user
RUN useradd --create-home dev && \
    usermod --append --groups sudo dev && \
    apt update && apt install -y sudo && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers

# install dumb init system
USER root
RUN apt update && apt install -y dumb-init

# setup oh-my-zsh
USER dev
ARG DOCKER_OHMYZSH_SCRIPT_NAME=zsh-in-docker.sh
ARG DOCKER_OHMYZSH_SCRIPT_URL=https://github.com/deluan/zsh-in-docker/releases/download/v1.1.3/${DOCKER_OHMYZSH_SCRIPT_NAME}
ARG DOCKER_OHMYZSH_SCRIPT_HASH="ffa8175332ef01b500ace59d03ce7e2f3a7453651e9a37060974bb6536f0706b  ${DOCKER_OHMYZSH_SCRIPT_NAME}"
RUN cd /tmp && \
    wget ${DOCKER_OHMYZSH_SCRIPT_URL} && \
    echo ${DOCKER_OHMYZSH_SCRIPT_HASH} | sha256sum -c && \
    chmod +x ./${DOCKER_OHMYZSH_SCRIPT_NAME} && \
    ./${DOCKER_OHMYZSH_SCRIPT_NAME} -t robbyrussell -p git -p ssh-agent && \
    rm ./${DOCKER_OHMYZSH_SCRIPT_NAME}

# install opam
USER dev
ARG OXCAML_REPO_URL=git+https://github.com/oxcaml/opam-repository.git
ARG OXCAM_VERSION=5.2.0+ox
RUN sudo apt update && sudo apt install opam autoconf libgmp-dev pkg-config zlib1g-dev -y && \
    opam init --disable-sandboxing -y && \
    opam update --all && \
    opam switch create ${OXCAM_VERSION} --repos ox=${OXCAML_REPO_URL},default && \
    eval $(opam env --switch ${OXCAM_VERSION})

USER dev
RUN opam install -y oxcaml_effect --no-checksums && \
    opam install -y \
      ocamlformat merlin ocaml-lsp-server utop parallel core_unix hardcaml hardcaml_test_harness \
      hardcaml_waveterm ppx_hardcaml core ppx_jane rope re dune

ENTRYPOINT ["/usr/bin/dumb-init", "--"]
