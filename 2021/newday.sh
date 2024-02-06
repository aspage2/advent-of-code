#!/bin/bash

set -e

if [ -z "$1" ] ; then
    echo "Usage: ./newday.sh [DAY #]"
    exit 1
fi

DIRNAME="days/$1"

if [ -d "$DIRNAME" -o -f "$DIRNAME" ] ; then
    echo "Already exists: $DIRNAME"
    exit 1
fi

mkdir $DIRNAME
cp -r .template/* $DIRNAME