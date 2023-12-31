#!/bin/bash
set -e

for day in day*; do
  if [ -f $day/al/main.al ]; then
    echo -n "Parsing $day AL script... "
    al parse $day/al/main.al
  fi
done
