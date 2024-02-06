use std::env;
use std::fmt::Display;
use std::fs;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Cell {
    Round,
    Square,
    Empty,
}

#[derive(Clone)]
struct Board {
    width: usize,
    height: usize,
    cells: Vec<Cell>,
}

impl PartialEq for Board {
    fn eq(&self, other: &Self) -> bool {
        if self.width != other.width || self.height != other.height {
            return false;
        }
        self.cells
            .iter()
            .zip(other.cells.iter())
            .all(|(x, y)| *x == *y)
    }
}

#[derive(Clone, Copy, Debug)]
enum Direction {
    North,
    South,
    East,
    West,
}

#[derive(Clone, Copy)]
struct BoardZipper {
    width: isize,
    height: isize,
    row: isize,
    col: isize,
}

impl BoardZipper {
    fn outside_limit(&self, d: Direction) -> bool {
        match d {
            Direction::North => self.row < 0,
            Direction::South => self.row > self.height - 1,
            Direction::East => self.col > self.width - 1,
            Direction::West => self.col < 0,
        }
    }
    fn move_to(&mut self, d: Direction) {
        if self.outside_limit(d) {
            return;
        }
        match d {
            Direction::North => {
                self.row -= 1;
            }
            Direction::South => {
                self.row += 1;
            }
            Direction::East => {
                self.col += 1;
            }
            Direction::West => {
                self.col -= 1;
            }
        }
    }
    fn set(&self, board: &mut Board, c: Cell) {
        *board.get_mut(self.row as usize, self.col as usize) = c;
    }
    fn get(&self, board: &Board) -> Cell {
        board.get(self.row as usize, self.col as usize)
    }
}

impl Board {
    fn parse(contents: &str) -> Board {
        let lines: Vec<&str> = contents.lines().collect();
        Board {
            width: lines[0].len(),
            height: lines.len(),
            cells: lines
                .iter()
                .flat_map(|l| {
                    l.bytes().map(|c| match c {
                        b'#' => Cell::Square,
                        b'.' => Cell::Empty,
                        b'O' => Cell::Round,
                        c => panic!("Not good: {}", c),
                    })
                })
                .collect(),
        }
    }

    fn zipper_at(&self, r: usize, c: usize) -> BoardZipper {
        BoardZipper {
            row: r as isize,
            col: c as isize,
            width: self.width as isize,
            height: self.height as isize,
        }
    }

    fn get(&self, r: usize, c: usize) -> Cell {
        self.cells[r * self.width + c]
    }

    fn get_mut(&mut self, r: usize, c: usize) -> &mut Cell {
        &mut self.cells[r * self.width + c]
    }

    fn tilt(&mut self, d: Direction) {
        let mut main_zipper: BoardZipper;
        let main_d: Direction;
        let acc_d: Direction;
        match d {
            Direction::North => {
                main_zipper = self.zipper_at(0, 0);
                main_d = Direction::East;
                acc_d = Direction::South;
            }
            Direction::South => {
                main_zipper = self.zipper_at(self.height - 1, 0);
                main_d = Direction::East;
                acc_d = Direction::North;
            }
            Direction::East => {
                main_zipper = self.zipper_at(0, self.width - 1);
                main_d = Direction::South;
                acc_d = Direction::West;
            }
            Direction::West => {
                main_zipper = self.zipper_at(0, 0);
                main_d = Direction::South;
                acc_d = Direction::East;
            }
        };
        while !main_zipper.outside_limit(main_d) {
            let mut acc_zipper = main_zipper.clone();
            while !acc_zipper.outside_limit(acc_d) {
                let mut fill_zipper = acc_zipper.clone();
                while !acc_zipper.outside_limit(acc_d) {
                    match acc_zipper.get(&self) {
                        Cell::Round => {
                            acc_zipper.set(self, Cell::Empty);
                            fill_zipper.set(self, Cell::Round);
                            fill_zipper.move_to(acc_d);
                        }
                        Cell::Empty => {}
                        Cell::Square => {
                            acc_zipper.move_to(acc_d);
                            break;
                        }
                    }
                    acc_zipper.move_to(acc_d);
                }
            }
            main_zipper.move_to(main_d);
        }
    }

    fn total_load(&self) -> usize {
        self.cells
            .iter()
            .enumerate()
            .filter_map(|(i, c)| {
                if *c != Cell::Round {
                    return None;
                }
                Some(self.height - i / self.width)
            })
            .sum()
    }
}

impl Display for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        for row in 0..self.height {
            for col in 0..self.width {
                s.push(match self.get(row, col) {
                    Cell::Empty => '.',
                    Cell::Square => '#',
                    Cell::Round => 'O',
                })
            }
            s.push('\n')
        }
        write!(f, "{}", s)
    }
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let mut b = Board::parse(&contents);

    let mut boards: Vec<Board> = Vec::new();
    boards.push(b.clone());
    let mut cycle_start: usize = 0;
    let mut cycle_end: usize = 0;

    // Continue performing cycles, keeping track of intermediate boards
    // until we reach a board we've seen before. The position of that
    // board up to (and exclusing) the current cycle # is a **test cycle**.
    //
    // With the incremental vec we track, the board state after N tests will
    // be (N - cycle_start) % (cycle_end - cycle_start + 1)
    for ind in 1.. {
        b.tilt(Direction::North);
        b.tilt(Direction::West);
        b.tilt(Direction::South);
        b.tilt(Direction::East);

        let pos = boards.iter().position(|b1| b1.eq(&b));
        boards.push(b.clone());
        if let Some(i) = pos {
            cycle_end = ind - 1;
            cycle_start = i;
            println!("Cycle from {} to {}", cycle_start, cycle_end);
            break;
        }
    }
    let cycle_size = cycle_end - cycle_start + 1;
    let cycle_pos = (1000000000 - cycle_start) % cycle_size;
    println!("{}", boards[cycle_start + cycle_pos].total_load());
}
