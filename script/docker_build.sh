#!/usr/bin/env bash

repo=atongen
name=setml

version=`cat ${name}.opam | egrep '^version: ' | cut -d '"' -f2`
image="${repo}/${name}:${version}"

docker build -t "$image" .
