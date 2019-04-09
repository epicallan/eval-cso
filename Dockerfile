FROM ubuntu:latest

LABEL maintainer="epicallan.al@gmail.com"

RUN mkdir /src

WORKDIR /src

RUN mkdir uploads

RUN apt-get update

RUN apt-get -y install wget unzip libgmp-dev

RUN wget https://github.com/epicallan/eval-cso/releases/download/0.1.1/eval-build.zip

RUN unzip -q deploy-build.zip

# copy prod.yaml from server into container
COPY config/prod.yaml ./src/config/

EXPOSE 8888 8080
