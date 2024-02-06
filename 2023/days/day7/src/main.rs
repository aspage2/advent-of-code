use std::env;
use std::fs;

fn strength(c: u8) -> u32 {
    match c {
        b'A' => 13,
        b'K' => 12,
        b'Q' => 11,
        b'T' => 10,
        b'9' => 9,
        b'8' => 8,
        b'7' => 7,
        b'6' => 6,
        b'5' => 5,
        b'4' => 4,
        b'3' => 3,
        b'2' => 2,
        b'J' => 1,
        _ => 0,
    }
}

type Hand = [u8; 5];

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum HandType {
    HighCard,
    OnePair,
    TwoPair,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

fn get_hand_type(h: &Hand) -> HandType {
    let mut counts: [u8; 13] = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
    let mut count_counts: [u8; 5] = [0, 0, 0, 0, 0];

    for &card in h {
        let ind = strength(card) - 1;
        counts[ind as usize] += 1;
    }

    for &count in &counts[1..] {
        if count == 0 {
            continue;
        }
        count_counts[(count - 1) as usize] += 1;
    }
    let num_jokers = counts[0];
    let max_set_size: u8 = count_counts
        .iter()
        .enumerate()
        .filter(|&(_, v)| *v != 0)
        .max_by_key(|(i, _)| *i)
        .map(|(i, _)| (i as u8) + 1)
        .unwrap_or_else(|| 0);

    println!(
        "{} max size: {} num jokers: {}",
        String::from_utf8(h.to_vec()).unwrap(),
        max_set_size,
        num_jokers
    );

    // If there are enough such that the number of
    // jokers can create a set of 5 or 4, that's
    // the best we can do.
    //
    // These are the only avenues where we need to
    // cover 4 of a kind or 5 of a kind.
    match max_set_size + num_jokers {
        5 => return HandType::FiveOfAKind,
        4 => return HandType::FourOfAKind,
        _ => {}
    }

    // If we have three of a kind at this point,
    // we can't have a joker card because it would
    // have been caught by the previous match.
    if max_set_size == 3 {
        // But there's still the chance that we can get a full house.
        if count_counts[1] > 0 {
            return HandType::FullHouse;
        }
        return HandType::ThreeOfAKind;
    }

    // We have two pair.
    if count_counts[1] == 2 {
        // Unless we have a joker; then it's a full house.
        if num_jokers == 1 {
            return HandType::FullHouse;
        }
        return HandType::TwoPair;
    } else if count_counts[1] == 1 {
        if num_jokers == 1 {
            return HandType::ThreeOfAKind;
        }
        return HandType::OnePair;
    }
    return match num_jokers {
        2 => HandType::ThreeOfAKind,
        1 => HandType::OnePair,
        _ => HandType::HighCard,
    };
}

fn compare_hands(h1: &Hand, h2: &Hand) -> std::cmp::Ordering {
    let ht1 = get_hand_type(h1);
    let ht2 = get_hand_type(h2);
    if ht1 > ht2 {
        return std::cmp::Ordering::Greater;
    } else if ht1 < ht2 {
        return std::cmp::Ordering::Less;
    }
    for (&c1, &c2) in h1.iter().zip(h2.iter()) {
        let (s1, s2) = (strength(c1), strength(c2));
        if s1 > s2 {
            return std::cmp::Ordering::Greater;
        } else if s1 < s2 {
            return std::cmp::Ordering::Less;
        }
    }
    return std::cmp::Ordering::Equal;
}

fn hand_from_str(s: &str) -> Hand {
    let bs: [u8; 5] = s.as_bytes().try_into().unwrap();
    return bs;
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let mut hands: Vec<(Hand, u32)> = contents
        .lines()
        .map(|s| s.split_whitespace().collect::<Vec<&str>>())
        .map(|p| (hand_from_str(p[0]), p[1].parse().unwrap()))
        .collect();

    hands.sort_by(|(h1, _), (h2, _)| compare_hands(h1, h2));
    for (hand, _) in &hands {
        println!(
            "Hand: {} ({:?})",
            String::from_utf8(hand.to_vec()).unwrap(),
            get_hand_type(&hand)
        );
    }

    let p: u32 = hands
        .iter()
        .enumerate()
        .map(|(i, (_, b))| (i as u32 + 1) * b)
        .sum();

    println!("{}", p);
}
