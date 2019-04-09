FROM fpco/stack-build:lts-12.26 as build

LABEL maintainer="epicallan.al@gmail.com"

RUN mkdir /opt/build
COPY . /opt/build
RUN cd /opt/build && stack build --system-ghc

FROM alpine:3.9 as runtime

RUN mkdir -p /opt/eval-cso

WORKDIR /opt/eval-cso

RUN wget https://github.com/epicallan/eval-cso/releases/download/v0.1.4/eval-build.zip

COPY --from=build /opt/build/.stack-work/install/x86_64-linux/lts-12.26/8.4.4/bin .

RUN mv /src/deploy/eval-ex /src/
RUN mv /src/deploy/eval-migrate /src/

ENV env=Production

# ensure you have prod.yaml from server into container
COPY config /src/config
