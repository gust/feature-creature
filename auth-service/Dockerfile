FROM ubuntu:14.04

RUN apt-get update && apt-get install -y \
    libpq-dev \
    libgmp-dev

RUN mkdir -p /usr/local/.auth-service
RUN mkdir -p $HOME/.auth-service

WORKDIR /usr/local/auth-service

COPY .stack-work/install /usr/local/auth-service
COPY env/development.env /usr/local/auth-service

RUN cp $(find /usr/local -regex '.*x86_64-linux.*auth-service-exe') /usr/local/bin
RUN cp development.env $HOME/.auth-service

CMD echo "auth-service setup completed"
