#!/usr/bin/env bash

cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"

# capture current env
tmpenv=$(mktemp /tmp/`basename $0`.XXXXXXXXXX)
trap "rm -f $tmpenv" EXIT
env | sed 's/=/="/' | sed 's/$/"/' > "$tmpenv"

# source envfile - medium priority
ENVFILE=${ENVFILE:-envfile}
[[ -f "$ENVFILE" ]] && source "${ENVFILE}"

# set default value if not already set
if [[ -z "$SETML_ENV" ]]; then
    SETML_ENV="development"
fi
if [[ -z "$DB_NAME" ]]; then
    DB_NAME="setml_${SETML_ENV}"
fi
if [[ -z "$DB_HOST" ]]; then
    DB_HOST="localhost"
fi
if [[ -z "$DB_PORT" ]]; then
    DB_PORT="5432"
fi
if [[ -z "$DB_USER" ]]; then
    DB_USER="`whoami`"
fi
if [[ -z "$DB_PASS" ]]; then
    DB_PASS="abcd1234"
fi
if [[ -z "$DB_POOL" ]]; then
    DB_POOL="`cat /proc/cpuinfo | grep processor | wc -l`"
fi

# re-source current env
source "$tmpenv"

echo "SETML_ENV=$SETML_ENV \
DB_NAME=$DB_NAME \
DB_HOST=$DB_HOST \
DB_PORT=$DB_PORT \
DB_USER=$DB_USER \
DB_PASS=$DB_PASS \
DB_POOL=$DB_POOL"
