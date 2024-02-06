use std::env;
use std::fs;

// Map word digits to the number they represent.
static DIGITS: [(&str, u32); 9] = [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
];

// parse_number_at_index parses the bytestring at the given index for
// either a single ascii digit or a lowercase digit "word".
// If either are found, the corresponding numerical digit is
// returned, otherwise None.
fn parse_number_at_index(s: &Vec<u8>, idx: usize) -> Option<u32> {
    if idx >= s.len() {
        return None;
    }
    if s[idx].is_ascii_digit() {
        return Some((s[idx] - b'0') as u32);
    }
    for (st, d) in DIGITS {
        if s[idx..].starts_with(st.as_bytes()) {
            return Some(d);
        }
    }
    return None;
}

// get_number_from_line parses the input string to find
// the first and last "digit" in the string. "digits" are
// any substring within the input string that are identified
// by the function parse_number_at_index.
fn get_number_from_line(s: &str) -> u32 {
    let s_bytes: Vec<u8> = s.bytes().collect();

    let mut first: Option<u32> = None;
    let mut last: Option<u32> = None;
    let mut idx: usize = 0;

    while idx < s_bytes.len() {
        let res = parse_number_at_index(&s_bytes, idx);
        if res.is_none() {
            continue;
        }
        if first.is_none() {
            first = res;
            last = res;
        } else {
            last = res;
        }
        idx += 1;
    }
    return 10 * first.unwrap_or_default() + last.unwrap_or_default();
}

fn main() {
    // Parse args
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        println!("Please enter filename");
        return;
    }
    // Read input file
    let contents = args
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("couldn't read file");

    // Sum up control values
    let mut total = 0;
    for line in contents.lines() {
        let num = get_number_from_line(line);
        println!("{} ({})", &line, &num);
        total += num;
    }

    println!("{}", total);
}
