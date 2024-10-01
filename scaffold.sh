#!/bin/sh

project_root=$(dirname "$0")

if [ $# != 1 ]
then
    echo "usage: $0 DAY_NUMBER" >&2
fi

day_dir="$project_root/day$1"
if [ -d "$day_dir" ]
then
    echo "day$1/ Already exists"
else
    cp -rd "$project_root/template" "$day_dir"
fi

