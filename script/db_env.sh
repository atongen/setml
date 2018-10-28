#!/usr/bin/env bash

cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

SETML_ENV=${SETML_ENV:-development}
DB_NAME=${DB_NAME:-setml_${SETML_ENV}}
DB_HOST=${DB_HOST:-localhost}
DB_PORT=${DB_PORT:-5432}
DB_USER=${DB_USER:-`whoami`}
DB_PASS=${DB_PASS:-abcd1234}
DB_POOL=${DB_POOL:-`cat /proc/cpuinfo | grep processor | wc -l`}

[[ -f envfile ]] && source envfile

echo "SETML_ENV=$SETML_ENV \
DB_NAME=$DB_NAME \
DB_HOST=$DB_HOST \
DB_PORT=$DB_PORT \
DB_USER=$DB_USER \
DB_PASS=$DB_PASS \
DB_POOL=$DB_POOL"
