#!/usr/bin/env bash
cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
eval `script/db_env.sh`
SETML_ENV="$SETML_ENV" \
DB_PASS="$DB_PASS" \
  bin/setml \
    --db-name "$DB_NAME" \
    --db-host "$DB_HOST" \
    --db-port "$DB_PORT" \
    --db-user "$DB_USER" \
    --db-pool "$DB_POOL"
