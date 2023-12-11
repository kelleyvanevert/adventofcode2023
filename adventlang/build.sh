#/bin/bash

name=${1:-al}

cargo build --release

echo
echo "Usage: $name run <file>"
cp ./target/release/adventlang ~/bin/$name
