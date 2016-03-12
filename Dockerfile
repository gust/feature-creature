FROM haskell:7.10.3

RUN apt-get update && apt-get install -y \
    git \
    libpq-dev

COPY . /usr/local/feature-creature

WORKDIR /usr/local/feature-creature

RUN mkdir .stack-work/bin
VOLUME [ "/root/.stack", "/usr/local/feature-creature/.stack-work" ]

RUN stack --local-bin-path=.stack-work/bin install
