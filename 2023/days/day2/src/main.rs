use std::env;
use std::fs;
use std::str;

fn take_integer(st: &Vec<u8>, start: usize, base: u32) -> (u32, usize) {
    let mut idx = start;
    while idx < st.len() && st[idx].is_ascii_digit() {
        idx += 1
    }
    let s = str::from_utf8(&st[start..idx]).unwrap();
    return (str::parse(s).unwrap(), idx);
}

fn parse_game(st: &Vec<u8>) -> Game {
    let mut idx = 5;

    let id: u32;
    (id, idx) = take_integer(&st, idx, 10);

    let mut rounds: Vec<(u32, u32, u32)> = Vec::new();

    idx += 2;
    while idx < st.len() {
        let mut r: u32 = 0;
        let mut g: u32 = 0;
        let mut b: u32 = 0;

        while idx < st.len() {
            let (count, new_idx) = take_integer(&st, idx, 10);
            idx = new_idx + 1;
            match st[idx] {
                b'r' => {
                    r = count;
                    idx += 3;
                }
                b'g' => {
                    g = count;
                    idx += 5;
                }
                b'b' => {
                    b = count;
                    idx += 4;
                }
                _ => panic!("nah"),
            }
            if idx >= st.len() {
                break;
            }
            idx += 2;
            if st[idx] == b';' {
                break;
            }
        }
        rounds.push((r, g, b));
    }

    return Game { id, rounds };
}

struct Game {
    id: u32,
    rounds: Vec<(u32, u32, u32)>,
}

impl Game {
    fn valid(&self) -> bool {
        self.rounds
            .iter()
            .all(|&(r, g, b)| r <= 12 && g <= 13 && b <= 14)
    }

    fn power(&self) -> u32 {
        let mut r: u32 = 0;
        let mut g: u32 = 0;
        let mut b: u32 = 0;

        for &(_r, _g, _b) in &self.rounds {
            if _r > r {
                r = _r;
            }
            if _g > g {
                g = _g;
            }
            if _b > b {
                b = _b;
            }
        }
        r * g * b
    }
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let ans: u32 = contents
        .lines()
        .map(|l| parse_game(&l.as_bytes().to_vec()))
        .map(|g| g.power())
        .sum();

    println!("{}", ans);
}
