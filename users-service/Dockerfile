FROM ubuntu:14.04

RUN apt-get update && apt-get install -y \
    libpq-dev \
    libgmp-dev

RUN mkdir -p /usr/local/.users-service
RUN mkdir -p $HOME/.users-service

WORKDIR /usr/local/users-service

COPY .stack-work/install /usr/local/users-service
COPY env/development.env /usr/local/users-service

RUN cp $(find /usr/local -regex '.*x86_64-linux.*users-service-exe') /usr/local/bin
RUN cp development.env $HOME/.users-service

CMD echo "users-service setup completed"
