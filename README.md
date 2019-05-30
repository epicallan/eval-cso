# REST API for a customer Service Officer quality evaluation tool

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Build Status](https://travis-ci.org/epicallan/eval-cso.svg?branch=master)](https://travis-ci.org/epicallan/eval-cso)

## Running app

Provide an environment variable to executable to run in either development or production mode eg


`$>  ENV=Production eval-cso`

Ensure you have created a config file in [config](./config) for the environment which you are running in i.e dev.yaml or test.yaml or prod.yaml.
You may as well declare app configs as environment variables see docker-compose.yaml.
