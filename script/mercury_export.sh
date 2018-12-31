#!/usr/bin/env bash

set -e

cd "$( cd "$( dirname "${BASH_SOURCE[0]}" )/.." && pwd )"
git gc --aggressive
dir=/home/atongen/tmp/setml

# Deploying to `Intel(R) Xeon(R) CPU E5504 @ 2.00GHz`
ssh mercury "rm -rf $dir && mkdir -p $dir"
git archive --format=tar origin/master | gzip -9c | ssh mercury "tar --directory=$dir -xvzf -"
scp -r ./.git mercury:$dir/
