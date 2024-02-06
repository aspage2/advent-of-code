use std::env;
use std::fs;
use std::iter::successors;

fn parse_line(line: &str) -> Vec<i32> {
    line.split_whitespace()
        .map(|s| s.parse().unwrap())
        .collect()
}

fn break_down(nums: Vec<i32>) -> Vec<Vec<i32>> {
    successors(Some(nums), |ns| {
        if ns.len() == 0 || ns.iter().all(|&x| x == 0) {
            None
        } else {
            Some(
                ns.iter()
                    .zip(ns.iter().skip(1))
                    .map(|(&a, &b)| b - a)
                    .collect(),
            )
        }
    })
    .collect()
}

fn predict_fw(steps: &Vec<Vec<i32>>) -> i32 {
    steps
        .iter()
        .skip(1)
        .fold(0, |acc, ns| acc + ns.last().unwrap())
}

fn predict_bw(steps: &Vec<Vec<i32>>) -> i32 {
    steps
        .iter()
        .skip(1)
        .fold(0, |acc, ns| ns.first().unwrap() - acc)
}

fn main() {
    let contents = env::args()
        .last()
        .and_then(|fname| fs::read_to_string(fname).ok())
        .expect("Couldn't read file");

    let mut tot_bw = 0;
    let mut tot_fw = 0;

    for line in contents.lines() {
        let mut steps = break_down(parse_line(line));
        steps.reverse();
        tot_fw += predict_fw(&steps);
        tot_bw += predict_bw(&steps);
    }
    println!("FW: {} BW: {}", tot_fw, tot_bw);
}
