#!/bin/bash

for day in day*; do
  echo
  echo "Executing $day..."
  echo "==="
  al run -t $day/al/main.al < $day/input.txt
done
