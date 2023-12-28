#!/bin/bash
set -e

if [[ $# -eq 0 ]] ; then
  echo "Please pass passphrase as argument"
  exit 1
fi

mkdir -p _inputs

for day in day*; do
  if [ -f $day/input.txt ]; then
    rm -rf _inputs/input.$day.txt.gpg
    gpg --passphrase $1 -c --no-symkey-cache --cipher-algo AES256 --batch -o _inputs/input.$day.txt.gpg $day/input.txt
    echo "Encrypted $day/input.txt -> _inputs/input.$day.txt.gpg"
  else
    echo "Input file missing: $day/input.txt"
  fi
done
