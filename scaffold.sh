#!/bin/sh

cd $(dirname "$0")

n=$1
sed -i "s/(names \(.*\))/(names \1 day$n)/" bin/dune
touch input/day$n.txt input/day$n-example.txt bin/day$n.ml
