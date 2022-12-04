use anyhow::Result;
use itertools::Itertools;

const _INPUT: &str = include_str!("../../data/input_04.txt");

const _TEST: &str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
2-5,15-90";

fn part_one(input: &str) -> i64 {
    let mut score = 0;
    for (a1, a2, b1, b2) in input.lines().map(|l| {
        l.split([',', '-'])
            .map(|v| v.parse::<i64>().unwrap())
            .collect_tuple::<(_, _, _, _)>()
            .unwrap()
    }) {
        if a1 <= b1 && a2 >= b2 {
            // a contains b
            score += 1;
        } else if b1 <= a1 && b2 >= a2 {
            // b contains a
            score += 1;
        }
    }
    score
}

fn part_two(input: &str) -> i64 {
    input
        .lines()
        .map(|l| {
            l.split(&[',', '-'])
                .map(|v| v.parse::<i64>().unwrap())
                .collect_tuple::<(_, _, _, _)>()
                .unwrap()
        })
        .filter(|(a, b, c, d)| a <= d && b >= c)
        .count()
        .try_into()
        .unwrap()
}

fn part_one_safe(input: &str) -> Result<i64> {
    let mut score = 0;
    for (a, b, c, d) in input.lines().filter_map(|line| {
        line.split([',', '-'])
            .filter_map(|v| v.parse::<i64>().ok()) // discard failed parses
            .collect_tuple::<(_, _, _, _)>() // discard failed rows
    }) {
        if a <= c && b >= d {
            // a contains b
            score += 1;
        } else if c <= a && d >= b {
            // b contains a
            score += 1;
        }
    }

    Ok(score)
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 1 test: {:?}", part_one_safe(_TEST));
    println!("part 1 input: {:?}", part_one_safe(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
