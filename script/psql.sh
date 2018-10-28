#!/usr/bin/env bash
cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
eval `script/db_env.sh`
PGPASSWORD="$DB_PASS" \
  psql \
  --host "$DB_HOST" \
  --port "$DB_PORT" \
  "$DB_NAME" "$DB_USER"
