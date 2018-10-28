#!/usr/bin/env bash
cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
script/psql.sh < sql/schema.sql
