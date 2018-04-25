#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "${DIR}/.." || exit 1

env="${1:-development}"
export PGCLUSTER="10/main"

psql "setml_${env}" < sql/schema.sql
