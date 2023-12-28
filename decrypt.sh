#!/bin/bash
set -e

if [[ $# -eq 0 ]] ; then
  echo "Please pass passphrase as argument"
  exit 1
fi

for day in day*; do
  if [ -f _inputs/input.$day.txt.gpg ]; then
    rm -rf $day/input.txt
    gpg --quiet --passphrase $1 -d --no-symkey-cache --cipher-algo AES256 --batch -o $day/input.txt _inputs/input.$day.txt.gpg
    echo "Decypted _inputs/input.$day.txt.gpg -> $day/input.txt"
  else
    echo "Encrypted input file missing: _inputs/input.$day.txt.gpg"
  fi
done
