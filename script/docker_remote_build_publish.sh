#!/usr/bin/env bash

ssh mercury "rm -rf /home/atongen/tmp/setml && mkdir -p /home/atongen/tmp/setml"
git archive --format=tar origin/master | gzip -9c | ssh mercury "tar --directory=/home/atongen/tmp/setml -xvzf -"
