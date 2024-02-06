#!/usr/bin/env bash


for i in $(seq 8 25) ; do
    mkdir -p days/$i
    test days/$i/main.exs && echo 'IO.puts("Hello, world")' > days/$i/main.exs
    touch days/$i/test.txt days/$i/sample.txt
done