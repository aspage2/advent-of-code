use std::collections::HashMap;
use std::env;
use std::fs;

#[derive(Debug)]
enum Direction {
    Right,
    Left,
}

struct Node {
    name: [u8; 3],
    left: usize,
    right: usize,
}

impl std::fmt::Display for Network {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for node in &self.nodes {
            let res = write!(
                f,
                "({} -> ({}, {}))\n",
                String::from_utf8(node.name.to_vec()).unwrap(),
                String::from_utf8(self.nodes[node.left].name.to_vec()).unwrap(),
                String::from_utf8(self.nodes[node.right].name.to_vec()).unwrap(),
            );
            if res.is_err() {
                return res;
            }
        }
        return Ok(());
    }
}

struct Network {
    name_map: HashMap<[u8; 3], usize>,
    nodes: Vec<Node>,
}

impl Network {
    fn lookup(&self, curr: usize, direction: &Direction) -> usize {
        let entry = &self.nodes[curr];
        match direction {
            Direction::Left => entry.left,
            Direction::Right => entry.right,
        }
    }
    fn ind_by_name(&self, name: [u8; 3]) -> usize {
        *self.name_map.get(&name).unwrap()
    }
}

fn parse_input(contents: &str) -> (Vec<Direction>, Network) {
    let mut lines = contents.lines();

    let directions: Vec<Direction> = lines
        .next()
        .unwrap()
        .bytes()
        .map(|c| {
            if c == b'R' {
                Direction::Right
            } else {
                Direction::Left
            }
        })
        .collect();

    lines.next();

    let network_lines: Vec<&str> = lines.collect();
    let mut nodes: Vec<Node> = Vec::with_capacity(network_lines.len());
    let mut name_to_ind: HashMap<[u8; 3], usize> = HashMap::new();

    for line in network_lines {
        let lb: Vec<u8> = line.as_bytes().into();
        let node_name: [u8; 3] = [lb[0], lb[1], lb[2]];
        let left_name: [u8; 3] = [lb[7], lb[8], lb[9]];
        let right_name: [u8; 3] = [lb[12], lb[13], lb[14]];

        for name in [node_name, left_name, right_name] {
            if !name_to_ind.contains_key(&name) {
                name_to_ind.insert(name, nodes.len());
                nodes.push(Node {
                    name,
                    left: 0,
                    right: 0,
                })
            }
        }

        let node: &mut Node = &mut nodes[*name_to_ind.get(&node_name).unwrap()];
        node.left = *name_to_ind.get(&left_name).unwrap();
        node.right = *name_to_ind.get(&right_name).unwrap();
    }

    return (
        directions,
        Network {
            nodes,
            name_map: name_to_ind,
        },
    );
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let (directions, network) = parse_input(&contents);

    let start: Vec<usize> = network
        .nodes
        .iter()
        .enumerate()
        .filter_map(|(i, n)| if n.name[2] == b'A' { Some(i) } else { None })
        .collect();

    let num_steps: usize = directions
        .iter()
        .cycle()
        .scan(start, |state, dir| {
            if state.iter().all(|&i| network.nodes[i].name[2] == b'Z') {
                return None;
            }
            for ind in state.iter_mut() {
                *ind = network.lookup(*ind, dir);
            }
            Some(state.len())
        })
        .count();

    println!("{}", num_steps);
}
