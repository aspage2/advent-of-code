use std::env;
use std::fs;

type Coord = (usize, usize);

struct Board<T> {
    width: usize,
    height: usize,
    nodes: Vec<T>,
}

impl<T> Board<T> {
    fn get(&self, (r, c): Coord) -> &T {
        &self.nodes[r * self.width + c]
    }
}
fn adjacent_connecting_pipes(board: &Board<Pipe>, (r, c): Coord) -> Vec<Coord> {
    let mut ret: Vec<Coord> = Vec::new();

    let this = board.get((r, c));

    if this.east && c < board.width {
        if board.get((r, c + 1)).west {
            ret.push((r, c + 1));
        }
    }
    if this.west && c > 0 {
        if board.get((r, c - 1)).east {
            ret.push((r, c - 1));
        }
    }
    if this.north && r > 0 {
        if board.get((r - 1, c)).south {
            ret.push((r - 1, c));
        }
    }
    if this.south && r < board.height {
        if board.get((r + 1, c)).north {
            ret.push((r + 1, c));
        }
    }
    ret
}

#[derive(Clone, Copy, Debug)]
struct Pipe {
    east: bool,
    west: bool,
    north: bool,
    south: bool,
}

impl Pipe {
    fn is_start(self) -> bool {
        self.east && self.west && self.north && self.south
    }
}

fn pipe_from_byte(b: u8) -> Pipe {
    match b {
        b'S' => Pipe {
            east: true,
            west: true,
            north: true,
            south: true,
        },
        b'-' => Pipe {
            east: true,
            west: true,
            north: false,
            south: false,
        },
        b'|' => Pipe {
            east: false,
            west: false,
            north: true,
            south: true,
        },
        b'.' => Pipe {
            east: false,
            west: false,
            north: false,
            south: false,
        },
        b'7' => Pipe {
            east: false,
            west: true,
            north: false,
            south: true,
        },
        b'J' => Pipe {
            east: false,
            west: true,
            north: true,
            south: false,
        },
        b'L' => Pipe {
            east: true,
            west: false,
            north: true,
            south: false,
        },
        b'F' => Pipe {
            east: true,
            west: false,
            north: false,
            south: true,
        },
        c => panic!("Didn't expect that: {}", c),
    }
}

fn parse_board(contents: &str) -> Board<Pipe> {
    let lines: Vec<&[u8]> = contents.lines().map(|l| l.as_bytes()).collect();

    Board {
        width: lines[0].len(),
        height: lines.len(),
        nodes: lines
            .iter()
            .flat_map(|b| b.into_iter().map(|b2| pipe_from_byte(*b2)))
            .collect(),
    }
}

fn find_start(board: &Board<Pipe>) -> Coord {
    for r in 0..board.height {
        for c in 0..board.width {
            if board.get((r, c)).is_start() {
                return (r, c);
            }
        }
    }
    panic!("Shit.")
}

fn resolve_start(board: &mut Board<Pipe>) {
    let (r, c) = find_start(board);
    let mut real = Pipe {
        north: false,
        south: false,
        east: false,
        west: false,
    };
    if r > 0 && board.get((r - 1, c)).south {
        real.north = true;
    }
    if r < board.height - 1 && board.get((r + 1, c)).north {
        real.south = true;
    }
    if c > 0 && board.get((r, c - 1)).east {
        real.west = true;
    }
    if c < board.width - 1 && board.get((r, c + 1)).west {
        real.east = true;
    }
    println!("Resolved start to {:?}", real);
    board.nodes[r * board.width + c] = real;
}

fn find_loop(board: &Board<Pipe>) -> Vec<Coord> {
    let start = find_start(&board);
    let mut path: Vec<Coord> = Vec::new();

    // XXX: This algorithm assumes that the Start space
    // only has two adjacent pipes trying to connect to it.
    // true for my inputs, but maybe not for others.
    path.push(start);

    while path.len() <= 1 || *path.last().unwrap() != start {
        let curr = path.last().unwrap();
        let pipes = adjacent_connecting_pipes(&board, *curr);
        let next = pipes
            .iter()
            .filter(|s| path.len() < 2 || **s != path[path.len() - 2])
            .next()
            .unwrap();
        path.push(*next);
    }
    path
}

fn count_inside(board: Board<Pipe>, path: &Vec<Coord>) -> u32 {
    type Cell = (bool, bool, bool, bool); // TL, TR, BL, BR

    let mut path_copy = path.clone();
    path_copy.sort();

    let mut tot: u32 = 0;
    let mut curr: Cell = (false, false, false, false);
    let mut prev: Cell;

    for r in 0..board.height {
        prev = (false, false, false, false);
        for c in 0..board.width {
            curr.0 = prev.1;
            curr.2 = prev.3;
            if path_copy.binary_search(&(r, c)).is_ok() {
                let pipe = board.get((r, c));
                curr.1 = curr.0 ^ pipe.north;
                curr.3 = curr.2 ^ pipe.south;
            }
            if curr == (true, true, true, true) {
                tot += 1;
            }
            prev = curr;
        }
    }
    tot
}

fn main() {
    let contents = env::args()
        .last()
        .or_else(|| Some(String::from("data/day10-sample.txt")))
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let mut board = parse_board(&contents);
    let path = find_loop(&board);
    resolve_start(&mut board);
    println!("{}", count_inside(board, &path));
}
