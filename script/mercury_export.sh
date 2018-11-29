#!/usr/bin/env bash

dir=/home/atongen/tmp/setml

ssh mercury "rm -rf $dir && mkdir -p $dir"
git archive --format=tar origin/master | gzip -9c | ssh mercury "tar --directory=$dir -xvzf -"
scp -r ./.git mercury:$dir/
