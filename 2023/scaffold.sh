#!/bin/sh

if [ $# != 1 ]; then
    echo "usage: $0 N" >&2
    exit 2
fi

cd $(dirname "$0")

n=$1
sed -i "s/\(day.*\))/\1\n  day$n)/" bin/dune
touch input/day$n.txt input/day$n-example.txt bin/day$n.ml
dune build
