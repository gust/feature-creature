FROM haskell:7.10.3

COPY . /usr/local/feature-creature

WORKDIR "/usr/local/feature-creature"

RUN apt-get update && apt-get install -y \
    git \
    libpq-dev

RUN stack build

# logging
RUN mkdir /usr/local/feature-creature/log
VOLUME [ "/usr/local/feature-creature/log" ]

RUN tee -a stack exec feature-creature-web /usr/local/feature-creature/log/api.log

CMD stack exec feature-creature-web
