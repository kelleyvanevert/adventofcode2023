#!/bin/bash
set -e

for day in day*; do
  if [ -f $day/al/main.al ]; then
    echo
    echo "Executing $day in AL..."
    echo "==="
    al run -t $day/al/main.al < $day/input.txt
  fi

  if [ -d $day/rust ]; then
    echo
    echo "Executing $day in Rust..."
    echo "==="
    (cd $day/rust; cargo run --release 2>/dev/null)
  fi
done
