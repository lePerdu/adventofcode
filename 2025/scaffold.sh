#!/bin/sh

dayN=day$1

cp -r template/ "$dayN"
sed -i "s/dayN/$dayN/" "$dayN/main.odin"
touch "$dayN/input.txt" "$dayN/example.txt"
