# Haskell examples

[![Build Status](https://travis-ci.org/falsandtru/haskell-examples.svg?branch=master)](https://travis-ci.org/falsandtru/haskell-examples)

## Test

1. `stack test`

## Container

Make container images from Alpine Linux.

1. `docker pull fpco/stack-build:lts-x`
1. `stack build`
1. `stack image container`

## Kubernetes

Serve a web service using k8s and Servant.

1. `stack image container`
1. `kubectl apply -f servant/manifest/`
