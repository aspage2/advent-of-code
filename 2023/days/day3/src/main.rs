use std::env;
use std::fs;

#[derive(Debug)]
struct Number {
    row: usize,
    col_start: usize,
    col_end: usize,
    value: u32,
}

#[derive(Debug)]
struct Symbol {
    value: char,
    row: usize,
    col: usize,
}

impl Number {
    fn adjacent(&self, other: &Symbol) -> bool {
        let dr = self.row as isize - other.row as isize;
        if dr < -1 || dr > 1 {
            return false;
        }

        let (cstart, cend, otherc) = (
            self.col_start as isize,
            self.col_end as isize,
            other.col as isize,
        );
        if dr == 0 {
            return cstart - otherc == 1 || otherc - cend == 0;
        }

        return otherc >= cstart - 1 && otherc <= cend;
    }
}

fn parse(contents: &str) -> (Vec<Number>, Vec<Symbol>) {
    let lines: Vec<&str> = contents.lines().collect();
    let row_len = lines[0].len();

    let mut numbers: Vec<Number> = Vec::new();
    let mut symbols: Vec<Symbol> = Vec::new();
    let mut scanning_num = false;

    for (r, line) in lines.into_iter().enumerate() {
        for (c, sym) in line.chars().enumerate() {
            if sym == '.' {
                if scanning_num {
                    scanning_num = false;
                    let n: &mut Number = numbers.last_mut().unwrap();
                    n.col_end = c;
                    n.value = line[n.col_start..n.col_end].parse().unwrap();
                }
                continue;
            }
            if sym.is_digit(10) {
                if !scanning_num {
                    scanning_num = true;
                    numbers.push(Number {
                        row: r,
                        col_start: c,
                        col_end: 0,
                        value: 0,
                    });
                }
                continue;
            }
            if scanning_num {
                scanning_num = false;
                let n: &mut Number = numbers.last_mut().unwrap();
                n.col_end = c;
                n.value = line[n.col_start..n.col_end].parse().unwrap();
            }

            symbols.push(Symbol {
                row: r,
                col: c,
                value: sym,
            });
        }
        if scanning_num {
            scanning_num = false;
            let n: &mut Number = numbers.last_mut().unwrap();
            n.col_end = row_len;
            n.value = line[n.col_start..n.col_end].parse().unwrap();
        }
    }

    return (numbers, symbols);
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let (nums, syms) = parse(&contents);

    let mut sum: u32 = 0;
    for num in &nums {
        for sym in &syms {
            if num.adjacent(sym) {
                sum += num.value;
                break;
            }
        }
    }
    println!("sum of adjacent numbers = {}", sum);

    sum = 0;
    for gear in syms.iter().filter(|&sym| sym.value == '*') {
        let (mut t, mut ct) = (1, 0);
        for num in &nums {
            if num.adjacent(&gear) {
                t *= num.value;
                ct += 1;
            }
        }
        if ct == 2 {
            sum += t;
        }
    }
    println!("{}", sum);
}
