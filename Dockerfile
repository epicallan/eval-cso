FROM ubuntu:latest

LABEL maintainer="epicallan.al@gmail.com"

RUN mkdir /src

WORKDIR /src

RUN apt-get update

RUN apt-get -y install wget unzip libgmp-dev libpq-dev postgresql-client-10

RUN wget https://github.com/epicallan/eval-cso/releases/download/v0.1.2/eval-build.zip

RUN unzip -q eval-build.zip

ENV env=Production

# copy prod.yaml from server into container

COPY .eval.yaml /.eval.yaml

EXPOSE 8888 8080
