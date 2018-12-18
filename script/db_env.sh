#!/usr/bin/env bash

cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

# capture current env
tmpenv=$(mktemp /tmp/`basename $0`.XXXXXXXXXX)
trap "rm -f $tmpenv" EXIT
env | sed 's/=/="/' | sed 's/$/"/' > "$tmpenv"

# set default values - lowest priority
SETML_ENV="development"
DB_NAME="setml_${SETML_ENV}"
DB_HOST="localhost"
DB_PORT="5432"
DB_USER="`whoami`"
DB_PASS="abcd1234"
DB_POOL="`cat /proc/cpuinfo | grep processor | wc -l`"

# source envfile - medium priority
ENVFILE=${ENVFILE:-envfile}
[[ -f "$ENVFILE" ]] && source "${ENVFILE}"

# source current env - highest priority
source "$tmpenv"

echo "SETML_ENV=$SETML_ENV \
DB_NAME=$DB_NAME \
DB_HOST=$DB_HOST \
DB_PORT=$DB_PORT \
DB_USER=$DB_USER \
DB_PASS=$DB_PASS \
DB_POOL=$DB_POOL"
