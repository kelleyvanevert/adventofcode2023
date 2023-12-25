#!/bin/bash

for day in day*; do
  if [ -f $day/al/main.al ]; then
    echo
    echo "Executing $day..."
    echo "==="
    al run -t $day/al/main.al < $day/input.txt
  fi
done
