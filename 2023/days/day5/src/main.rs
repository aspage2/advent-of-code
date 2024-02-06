mod parse;

use parse::Range;
use std::env;
use std::fs;

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let (seeds, maps) = parse::parse_contents(&contents);
    let mut ranges = seeds;

    for map in maps {
        let mut new_ranges: Vec<Range> = Vec::new();
        for &range in &ranges {
            let mapped = map.map_range(range);
            new_ranges.extend(mapped.iter());
        }
        ranges = new_ranges;
    }
    println!("{:?}", ranges.iter().min_by_key(|r| r.start));
}
