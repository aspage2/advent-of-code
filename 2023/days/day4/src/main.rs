use std::collections::hash_map;
use std::env;
use std::fs;

fn shits() -> Result<String, dyn std::error::Error> {
    fs::read_to_string("Hello.txt")
}

fn parse(line: &str) -> (Vec<u8>, Vec<u8>) {
    let words: Vec<&str> = line.split(' ').filter(|s| s.len() != 0).skip(2).collect();

    let sep = words.iter().position(|w| w == &"|").unwrap();

    let winning: Vec<u8> = words[..sep].iter().map(|w| w.parse().unwrap()).collect();

    let mine: Vec<u8> = words[sep + 1..]
        .iter()
        .map(|w| w.parse().unwrap())
        .collect();

    (winning, mine)
}

fn num_matching(winning: &Vec<u8>, mine: &Vec<u8>) -> u32 {
    let mut n = 0;
    for w in winning {
        for m in mine {
            if m == w {
                n += 1;
            }
        }
    }
    return n;
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let mut cards: Vec<(usize, u32)> = contents
        .lines()
        .map(parse)
        .map(|(w, m)| num_matching(&w, &m))
        .enumerate()
        .collect();

    cards.reverse();
    let mut tot: u32 = 0;

    let mut dp: hash_map::HashMap<usize, u32> = hash_map::HashMap::new();

    for (card_num, num_match) in cards {
        println!("Processing: Card {}, {} matches", card_num, num_match);
        let mut num = 1;
        for i in card_num + 1..card_num + 1 + num_match as usize {
            // We can assert the dp entry exists because
            // the puzzle guarantees that cards near the end won't
            // fall off the edge.
            let n = dp.get(&i).unwrap();
            num += n;
            println!(
                "Card {} wins Card {} (will produce {} more cards)",
                card_num, i, n
            );
        }
        dp.insert(card_num, num);
        tot += num;
    }

    println!("{}", tot);
}
