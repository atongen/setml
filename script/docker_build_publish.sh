#!/usr/bin/env bash

name=setml
version=`cat ${NAME}.opam | egrep '^version: ' | cut -d '"' -f2`
image="`whoami`/${name}:${version}"

docker build -t "$image" . && \
  docker push "$image"
