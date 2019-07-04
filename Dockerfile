# BUILDER
##############################################################################

FROM ubuntu:18.04 AS builder
LABEL author="Oleg Grenrus <oleg.grenrus@iki.fi>"

# A path we work in
WORKDIR /build

# Update APT
RUN apt-get -yq update && apt-get -yq upgrade

# hvr-ppa, provides GHC and cabal-install
RUN apt-get -yq --no-install-suggests --no-install-recommends install \
    software-properties-common \
    apt-utils \
  && apt-add-repository -y "ppa:hvr/ghc"

# Locales
# - UTF-8 is good
RUN apt-get -yq --no-install-suggests --no-install-recommends install \
    locales

RUN locale-gen en_US.UTF-8
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Some what stable dependencies
# - separately, mostly to spot ghc and cabal-install
RUN apt-get -yq --no-install-suggests --no-install-recommends install \
    cabal-install-2.4 \
    ghc-8.4.4 \
    ghc-8.6.5 \
    git

# More dependencies, all the -dev libraries
# - some basic collection of often needed libs
# - also some dev tools
RUN apt-get -yq --no-install-suggests --no-install-recommends install \
    build-essential \
    ca-certificates \
    curl \
    git \
    libgmp-dev \
    liblapack-dev \
    liblzma-dev \
    libpq-dev \
    libyaml-dev \
    netbase \
    openssh-client \
    pkg-config \
    zlib1g-dev

# Set up PATH
ENV PATH=/cabal/bin:/opt/ghc/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

# cabal-install configuration
# - we'll be in better control of the build environment, than with default config.
COPY docker.cabal.config /build/cabal.config
ENV CABAL_CONFIG /build/cabal.config

# Update cabal-install database
RUN cabal v2-update

# Install cabal-plan
# - we'll need it to find build artifacts
# - note: actual build tools ought to be specified in build-tool-depends field
RUN cabal v2-install cabal-plan --constraint='cabal-plan ^>=0.5' --constraint='cabal-plan +exe'

# Add a .cabal file to build environment
# - it's enough to build dependencies
COPY *.cabal cabal.project /build/

# Build package dependencies first
# - beware of https://github.com/haskell/cabal/issues/6106
RUN cabal v2-build -v1 --dependencies-only all

# Add rest of the files into build environment
# - remember to keep .dockerignore up to date
COPY . /build

# An executable to build
ARG EXECUTABLE

# Check that ARG is set up
RUN if [ -z "$EXECUTABLE" ]; then echo "ERROR: Empty $EXECUTABLE"; false; fi

# BUILD!!!
RUN cabal v2-build -v1 exe:$EXECUTABLE

# Copy build artifact to known directory
# - todo arg
RUN mkdir -p /build/artifacts && cp $(cabal-plan list-bin $EXECUTABLE) /build/artifacts/

# Make a final binary a bit smaller
RUN strip /build/artifacts/$EXECUTABLE; done

# Small debug output
RUN ls -lh /build/artifacts

# DEPLOYMENT IMAGE
##############################################################################

FROM ubuntu:18.04
LABEL author="Oleg Grenrus <oleg.grenrus@iki.fi>"

# Dependencies
# - no -dev stuff
# - cleanup apt stuff after installation
RUN apt-get -yq update && apt-get -yq --no-install-suggests --no-install-recommends install \
    ca-certificates \
    curl \
    libgmp10 \
    liblapack3 \
    liblzma5 \
    libpq5 \
    libyaml-0-2 \
    netbase \
    openssh-client \
    zlib1g \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# Working directory
WORKDIR /app

# Expose port
EXPOSE 8000

# Inherit the executable argument
ARG EXECUTABLE

# Copy build artifact from a builder stage
COPY --from=builder /build/artifacts/$EXECUTABLE /app/$EXECUTABLE

# ARG env isn't preserved, so we make another ENV
ENV EXECUTABLE_ $EXECUTABLE

RUN env
RUN ls /app

# Set up a default command to run
ENTRYPOINT /app/${EXECUTABLE_}
