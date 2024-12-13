#!/bin/sh

cat > src/day$1.gleam <<- EOF
import gleam/io

import util

pub fn main(day: util.AdventDay) {
	io.debug("Hello, AdventOfCode!")
}
EOF
