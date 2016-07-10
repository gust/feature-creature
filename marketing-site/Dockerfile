FROM ubuntu:14.04

RUN apt-get update && apt-get install -y \
    libpq-dev \
    libgmp-dev

RUN mkdir -p /usr/local/.marketing-site
RUN mkdir -p $HOME/.marketing-site

WORKDIR /usr/local/marketing-site

COPY .stack-work/install /usr/local/marketing-site
COPY env/development.env /usr/local/marketing-site

RUN cp $(find /usr/local -regex '.*x86_64-linux.*marketing-site-exe') /usr/local/bin
RUN cp development.env $HOME/.marketing-site

CMD echo "marketing-site setup completed"
