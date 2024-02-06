use std::env;
use std::fmt::Display;
use std::fs;

type LensBox = Vec<(String, u32)>;

fn hash(s: &str) -> u32 {
    let mut acc: u32 = 0;

    for b in s.bytes() {
        acc += b as u32;
        acc *= 17;
        acc = acc % 256;
    }
    acc
}

#[derive(Debug)]
enum Operation {
    Set(String, u32),
    Drop(String),
}

impl Operation {
    fn parse(data: &str) -> Operation {
        let mut i = 0;
        let bytes = data.as_bytes();
        while i < bytes.len() && bytes[i] != b'-' && bytes[i] != b'=' {
            i += 1;
        }
        let label = String::from_utf8(bytes[..i].to_vec()).unwrap();
        match bytes[i] {
            b'-' => Operation::Drop(label),
            b'=' => Operation::Set(label, (bytes[i + 1] - b'0') as u32),
            _ => panic!("nah."),
        }
    }
}

struct Boxes {
    boxes: Vec<LensBox>,
}

impl Boxes {
    fn new() -> Boxes {
        let mut boxes = Vec::new();
        for _ in 0..256 {
            boxes.push(Vec::new());
        }
        Boxes { boxes }
    }

    fn total_focus_power(&self) -> u32 {
        let mut tot: u32 = 0;
        for (i, lens_box) in self.boxes.iter().enumerate() {
            for (j, lens) in lens_box.iter().enumerate() {
                tot += (i as u32 + 1) * (j as u32 + 1) * lens.1
            }
        }
        tot
    }

    fn get_mut(&mut self, label: &String) -> &mut LensBox {
        &mut self.boxes[hash(&label) as usize]
    }

    fn apply(&mut self, op: &Operation) {
        match op {
            Operation::Set(label, focal_length) => {
                let lens_box = self.get_mut(&label);
                for (lens_label, lens_focal_length) in lens_box.iter_mut() {
                    if *lens_label == *label {
                        *lens_focal_length = *focal_length;
                        return;
                    }
                }
                lens_box.push((label.clone(), *focal_length));
            }
            Operation::Drop(label) => {
                let lens_box = self.get_mut(&label);
                let pos = lens_box.iter().position(|(l, _)| *l == *label);
                if let Some(i) = pos {
                    lens_box.remove(i);
                }
            }
        }
    }
}

impl Display for Boxes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut disp: String = String::new();

        for (i, lens_box) in self.boxes.iter().enumerate() {
            if lens_box.len() == 0 {
                continue;
            }
            disp.push_str(&format!("Box {}: ", i));
            for (l, fl) in lens_box {
                disp.push_str(&format!("[{} {}] ", l, fl));
            }
            disp.push('\n');
        }
        write!(f, "{}", disp)
    }
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let ops: Vec<Operation> = contents.split(',').map(Operation::parse).collect();

    let mut boxes = Boxes::new();

    for op in &ops {
        boxes.apply(&op);
    }
    println!("{}", boxes.total_focus_power())
}
