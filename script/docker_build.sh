#!/usr/bin/env bash

name=setml
version=`cat ${name}.opam | egrep '^version: ' | cut -d '"' -f2`
image="`whoami`/${name}:${version}"

docker build -t "$image" .
