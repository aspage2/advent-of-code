use std::env;
use std::fs;

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let lines: Vec<&str> = contents.lines().collect();

    let height = lines.len();
    let width = lines[0].len();

    let mut columns: Vec<bool> = std::iter::repeat(false).take(width).collect();

    let mut rows: Vec<bool> = std::iter::repeat(false).take(height).collect();

    let mut galaxies: Vec<(usize, usize)> = Vec::with_capacity(height * width / 2);

    // First pass: Determine empty rows & columns
    for r in 0..height {
        let l = lines[r].as_bytes();
        for c in 0..width {
            match l[c] {
                b'#' => {
                    columns[c] = true;
                    rows[r] = true;
                    galaxies.push((r, c));
                }
                b'.' => {}
                c => panic!("didn't expect this in input: {}", c),
            }
        }
    }
    let empty_rows: Vec<usize> = rows
        .iter()
        .enumerate()
        .filter_map(|(i, j)| if !j { Some(i) } else { None })
        .collect();

    let empty_columns: Vec<usize> = columns
        .iter()
        .enumerate()
        .filter_map(|(i, j)| if !j { Some(i) } else { None })
        .collect();

    // Second pass: map galaxies to their expanded coords
    let mult: usize = 1000000;
    for (r, c) in galaxies.iter_mut() {
        // the "empty row/column" vecs are guaranteed not to
        // have coordinates containing galaxies, so binary search
        // will always return an Err-value. Because the Err represents
        // the index where the value would be, we can use that Err
        // value as the # of empty rows/cols that come before this number.
        if let Err(n) = empty_rows.binary_search(r) {
            *r += n * (mult - 1);
        }
        if let Err(n) = empty_columns.binary_search(c) {
            *c += n * (mult - 1);
        }
    }

    let mut tot = 0;
    for i in 0..galaxies.len() {
        for j in i..galaxies.len() {
            tot += taxicab_dist(galaxies[i], galaxies[j]);
        }
    }
    println!("{}", tot);
}

fn taxicab_dist(c1: (usize, usize), c2: (usize, usize)) -> usize {
    c1.0.abs_diff(c2.0) + c1.1.abs_diff(c2.1)
}
