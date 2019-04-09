FROM fpco/stack-build:lts-12.26 as build

LABEL maintainer="epicallan.al@gmail.com"

RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM alpine:3.9 as runtime

RUN mkdir -p /opt/eval-cso

WORKDIR /opt/eval-cso

RUN apk --no-cache add ca-certificates gmp libffi libpq postgresql-dev

COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.26/8.4.4/bin .

COPY config /opt/eval-cso/config

ENV env=Development

EXPOSE 8888 8080
