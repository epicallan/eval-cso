FROM ubuntu:18.04

LABEL maintainer="epicallan.al@gmail.com"

RUN mkdir /src

WORKDIR /src

RUN apt-get update

RUN apt-get -y install wget unzip libgmp-dev libpq-dev

RUN wget https://github.com/epicallan/eval-cso/releases/download/v0.2.6/eval-build.zip

RUN unzip -q eval-build.zip

RUN mv /src/deploy/eval-ex /src/
RUN mv /src/deploy/eval-migrate /src/

# ensure you have prod.yaml from server into container
COPY config /src/config
