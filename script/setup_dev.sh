#!/usr/bin/env bash

set -e

opam update
opam switch setml --alias-of 4.06.1
eval `opam config env`

opam install \
  "jbuilder=1.0+beta20" \
  "caqti=0.11.0" \
  "caqti-driver-postgresql=0.11.0" \
  "caqti-lwt=0.11.0" \
  "cohttp=1.1.0" \
  "cohttp-lwt=1.0.2" \
  "cohttp-lwt-unix=1.0.2" \
  "containers=2.2" \
  "cow=2.3.0" \
  "merlin=3.0.5" \
  "nocrypto=0.5.4" \
  "postgresql=4.3.0" \
  "re=1.7.3" \
  "websocket=2.10" \
  "websocket-lwt=2.11" \
  "yojson=1.4.1" \
  "ounit=2.0.8"

