#!/usr/bin/env bash

repo=atongen
name=setml

version=`cat ${name}.opam | egrep '^version: ' | cut -d '"' -f2`
image="${repo}/${name}:${version}"

docker login -u "$repo" && \
# Deploying to `Intel(R) Xeon(R) CPU E5504 @ 2.00GHz`, which is old
# and apparently doesn't like "modernity". SEE:
# https://github.com/ocaml/opam/issues/2247
# https://github.com/mirleft/ocaml-nocrypto/issues/72
docker build --build-arg modernity="false" -t "$image" . && \
docker push "$image"
