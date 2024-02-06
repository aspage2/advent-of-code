use std::env;
use std::fs;

struct Board {
    width: usize,
    height: usize,
    cells: Vec<bool>,
}

impl Board {
    fn parse(contents: &str) -> Vec<Board> {
        let mut ret = Vec::new();
        let mut curr_width = 0;
        let mut curr_height = 0;
        let mut curr: Vec<bool> = Vec::new();

        for line in contents.lines() {
            if line == "" {
                ret.push(Board {
                    width: curr_width,
                    height: curr_height,
                    cells: curr,
                });
                curr = Vec::new();
                curr_width = 0;
                curr_height = 0;
                continue;
            }
            curr.extend(line.bytes().map(|c| c == b'#'));
            if curr_width == 0 {
                curr_width = curr.len();
            }
            curr_height += 1;
        }
        ret.push(Board {
            width: curr_width,
            height: curr_height,
            cells: curr,
        });
        ret
    }

    fn get(&self, r: usize, c: usize) -> bool {
        self.cells[r * self.width + c]
    }

    fn get_row(&self, r: usize) -> u64 {
        let mut ret = 0;
        for c in 0..self.width {
            ret = (ret << 1) | if self.get(r, c) { 1 } else { 0 };
        }
        ret
    }

    fn get_col(&self, c: usize) -> u64 {
        let mut ret = 0;
        for r in 0..self.height {
            ret = (ret << 1) | if self.get(r, c) { 1 } else { 0 };
        }
        ret
    }

    fn row_reflection(&self) -> Option<usize> {
        let mut rows = Vec::new();
        for r in 0..self.height {
            rows.push(self.get_row(r));
        }
        let ret = mirror_center(&rows);
        ret
    }

    fn col_reflection(&self) -> Option<usize> {
        let mut cols = Vec::new();
        for r in 0..self.width {
            cols.push(self.get_col(r));
        }
        let ret = mirror_center(&cols);
        ret
    }
}

fn mirror_center(nums: &Vec<u64>) -> Option<usize> {
    let mut max = None;
    let l = nums.len();
    for curr in 0..l - 1 {
        let mut lo = curr;
        let mut bad = false;
        let mut hi = curr + 1;
        let mut fixed_smudge = false;

        loop {
            if nums[lo] != nums[hi] {
                let mask = nums[lo] ^ nums[hi];
                if fixed_smudge || (mask == 0 || (mask & (mask - 1)) != 0) {
                    bad = true;
                    break;
                }
                fixed_smudge = true;
            }
            if lo == 0 || hi == l - 1 {
                break;
            }
            lo -= 1;
            hi += 1;
        }
        if !bad && fixed_smudge {
            max = Some((curr + 1) as usize);
        }
    }
    max
}

fn print_with_col_mirror(b: &Board, col: usize) {
    let mut data: String = String::new();

    data.extend(std::iter::repeat(' ').take(col - 1));
    data.push_str("><\n");

    for r in 0..b.height {
        for c in 0..b.width {
            if b.get(r, c) {
                data.push('#');
            } else {
                data.push('.');
            }
        }
        data.push('\n');
    }
    println!("{}", data);
}

fn print_with_row_mirror(b: &Board, row: usize) {
    let mut data: String = String::new();

    for r in 0..b.height {
        if r == row - 1 {
            data.push('v');
        } else if r == row {
            data.push('^');
        } else {
            data.push(' ');
        }
        for c in 0..b.width {
            if b.get(r, c) {
                data.push('#');
            } else {
                data.push('.');
            }
        }
        data.push('\n');
    }
    println!("{}", data);
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let boards = Board::parse(&contents);
    let mut sum = 0;
    for board in boards {
        if let Some(rr) = board.row_reflection() {
            sum += 100 * rr;
            println!("Found row reflection: {}", rr);
            print_with_row_mirror(&board, rr);
        }
        if let Some(cr) = board.col_reflection() {
            sum += cr;
            println!("Found col reflection: {}", cr);
            print_with_col_mirror(&board, cr);
        }
    }
    println!("{}", sum);
}
