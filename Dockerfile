FROM haskell:7.10.3

WORKDIR "$HOME/feature-creature"
COPY . "$HOME/feature-creature"

RUN apt-get update && apt-get install -y \
    git \
    libpq-dev

RUN stack setup
RUN stack build
