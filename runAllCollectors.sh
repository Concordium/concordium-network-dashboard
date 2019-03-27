#!/bin/bash

set -m # Enable Job Control

function cleanup {
  echo "Killing all 'dashboard-collector' processes"
  # Couldn't get pgrep to work for some reason, so here we are munging together the same functionality manually
  ps aux | grep dashboard-collector | grep -v grep | tr -s ' ' | cut -d' ' -f2 | xargs kill
}
trap cleanup EXIT

# Start a dashboard collector for each baker's exposed gRPC port
for i in `docker ps | grep 10000 | tr -s ' ' | cut -d' ' -f 11 | cut -d':' -f2 | cut -d'-' -f1`; do
  COLLECTOR_NAME=testnode$i node dashboard-collector -h localhost:$i &
done

# Wait for all parallel jobs to finish
while [ 1 ]; do fg 2> /dev/null; [ $? == 1 ] && break; done
