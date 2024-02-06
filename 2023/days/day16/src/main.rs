use std::collections::{HashSet, VecDeque};
use std::env;
use std::fs;

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum Cell {
    Splitter(bool),
    Reflector(bool),
    Empty,
}

impl Cell {
    // Calculate the beam(s) produced by the incident direction
    // hitting this cell.
    fn handle(&self, d: Direction) -> Vec<Direction> {
        match self {
            Cell::Empty => vec![d],
            Cell::Splitter(horiz) if *horiz == d.horizontal() => vec![d],
            Cell::Splitter(_) => vec![d.left(), d.right()],
            Cell::Reflector(nw) if *nw == d.horizontal() => vec![d.left()],
            _ => vec![d.right()],
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    fn move_to(&self, (r, c): Coord, board: &Board) -> Option<Coord> {
        match self {
            Direction::North if r == 0 => None,
            Direction::North => Some((r - 1, c)),
            Direction::South if r == board.height - 1 => None,
            Direction::South => Some((r + 1, c)),
            Direction::West if c == 0 => None,
            Direction::West => Some((r, c - 1)),
            Direction::East if c == board.width - 1 => None,
            Direction::East => Some((r, c + 1)),
        }
    }
    fn horizontal(&self) -> bool {
        match self {
            Direction::North | Direction::South => false,
            _ => true,
        }
    }
    fn left(&self) -> Direction {
        match *self {
            Direction::East => Direction::North,
            Direction::North => Direction::West,
            Direction::West => Direction::South,
            Direction::South => Direction::East,
        }
    }
    fn right(&self) -> Direction {
        match *self {
            Direction::East => Direction::South,
            Direction::North => Direction::East,
            Direction::West => Direction::North,
            Direction::South => Direction::West,
        }
    }
    fn opposite(&self) -> Direction {
        match *self {
            Direction::East => Direction::West,
            Direction::North => Direction::South,
            Direction::West => Direction::East,
            Direction::South => Direction::North,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Debug)]
struct Board {
    width: usize,
    height: usize,
    cells: Vec<Cell>,
}

type Coord = (usize, usize);

impl Board {
    fn parse(contents: &str) -> Board {
        let lines: Vec<&str> = contents.lines().collect();

        Board {
            width: lines[0].len(),
            height: lines.len(),
            cells: lines
                .iter()
                .flat_map(|line| {
                    line.bytes().map(|c| match c {
                        b'/' => Cell::Reflector(true),
                        b'\\' => Cell::Reflector(false),
                        b'-' => Cell::Splitter(true),
                        b'|' => Cell::Splitter(false),
                        b'.' => Cell::Empty,
                        c => panic!("Non-parseable char: {}", c),
                    })
                })
                .collect(),
        }
    }

    fn get(&self, (r, c): Coord) -> Cell {
        self.cells[r * self.width + c]
    }

    // Breadth-first traversal.
    // Each node is the coordinate pair PLUS the incident direction
    // of the beam entering the cell.
    fn explore(&self, start: Coord, indicdent: Direction) -> usize {

        let mut explore_set: VecDeque<(Coord, Direction)> = VecDeque::new();
        let mut visited: HashSet<(Coord, Direction)> = HashSet::new();

        explore_set.push_back((start, indicdent));

        while explore_set.len() > 0 {
            let (coord, incident) = explore_set.pop_front().unwrap();
            visited.insert((coord, incident));
            for new_direction in self.get(coord).handle(incident) {
                if let Some(new_c) = new_direction.move_to(coord, self) {
                    if !visited.contains(&(new_c, new_direction))
                        && !explore_set.contains(&(new_c, new_direction))
                    {
                        explore_set.push_back((new_c, new_direction));
                    }
                }
            }
        }

        let hs: HashSet<Coord> = HashSet::from_iter(visited.iter().map(|p| p.0));
        Vec::from_iter(hs.into_iter()).len()
    }
}

fn main() {
    let contents = env::args()
        .last()
        .or_else(|| Some(String::from("data/day10-sample.txt")))
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let b = Board::parse(&contents);

    let vertical_walls = (0..b.height)
        .map(|i| {
            b.explore((i, 0), Direction::East)
                .max(b.explore((i, b.width - 1), Direction::West))
        })
        .max()
        .unwrap();

    let horiz_walls = (0..b.width)
        .map(|i| {
            b.explore((0, i), Direction::South)
                .max(b.explore((b.height - 1, i), Direction::North))
        })
        .max()
        .unwrap();

    println!("{}", vertical_walls.max(horiz_walls));
}
