use std::env;
use std::fs;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
enum State {
    Operational,
    Damaged,
    Unknown,
}

#[derive(Debug)]
struct Record {
    data: Vec<State>,
}

impl Record {
    fn complete(&self) -> bool {
        !self.data.iter().any(|x| *x == State::Unknown)
    }

    fn to_string(&self) -> String {
        let mut ret = String::new();

        ret.extend(self.data.iter().map(|&c| match c {
            State::Unknown => '?',
            State::Damaged => '#',
            State::Operational => '.',
        }));
        ret
    }

    fn satisfies(&self, hints: &Vec<u32>) -> bool {
        let mut scan_state = State::Operational;
        let mut count_damaged = 0;
        let mut curr_hint = 0;

        for s in &self.data {
            match (scan_state, *s) {
                (State::Operational, State::Damaged) => {
                    scan_state = State::Damaged;
                    count_damaged = 1;
                    // We've entered another section of "damaged" springs
                    // but the hints say there are none.
                    if curr_hint >= hints.len() {
                        return false;
                    }
                }
                (State::Damaged, State::Operational) => {
                    scan_state = State::Operational;
                    // The current block doesn't match what we want.
                    if count_damaged != hints[curr_hint] {
                        return false;
                    }
                    curr_hint += 1;
                }
                (_, State::Unknown) => return true,
                (State::Operational, State::Operational) => {}
                (State::Damaged, State::Damaged) => {
                    count_damaged += 1;
                }
                (_, _) => panic!("what?"),
            }
        }

        match *self.data.last().unwrap() {
            State::Operational => curr_hint >= hints.len(),
            State::Damaged => curr_hint == hints.len() - 1 && count_damaged == hints[curr_hint],
            _ => false,
        }
    }

    fn from_string(s: &str) -> Record {
        Record {
            data: s
                .bytes()
                .map(|c| match c {
                    b'#' => State::Damaged,
                    b'.' => State::Operational,
                    b'?' => State::Unknown,
                    c => panic!("Not a char: {}", c),
                })
                .collect(),
        }
    }
}

fn parse(line: &str) -> (Record, Vec<u32>) {
    let split: Vec<&str> = line.split_whitespace().collect();
    return (
        Record::from_string(&split[0].repeat(5)),
        split[1]
            .split(|c| c == ',')
            .map(|s| s.parse().unwrap())
            .collect(),
    );
}

fn count_arrangements(r: &Record, v: &Vec<u32>) -> u32 {
    if !r.satisfies(v) {
        return 0;
    }
    if r.complete() {
        return 1;
    }

    let mut cpy = Record {
        data: r.data.clone(),
    };
    let i = cpy
        .data
        .iter()
        .enumerate()
        .filter(|&(_, c)| *c == State::Unknown)
        .next()
        .unwrap()
        .0;

    let mut tot = 0;
    cpy.data[i] = State::Operational;
    tot += count_arrangements(&cpy, &v);
    cpy.data[i] = State::Damaged;
    tot += count_arrangements(&cpy, &v);
    return tot;
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let tot: u32 = contents
        .lines()
        .map(|l| {
            let (r, h) = parse(l);
            count_arrangements(&r, &h.repeat(5))
        })
        .sum();

    println!("tot: {}", tot);
}
