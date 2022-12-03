use std::collections::HashSet;

use itertools::Itertools;

const _INPUT: &str = include_str!("../../data/input_03.txt");

const _TEST: &str = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw";

fn priority(c: char) -> i64 {
    if c.is_ascii_lowercase() {
        return c as i64 - b'a' as i64 + 1;
    } else {
        return c as i64 - b'A' as i64 + 27;
    }
}

fn part_one(input: &str) -> i64 {
    let mut score = 0;
    for line in input.lines() {
        let (l, r) = line.split_at(line.len() / 2);
        let ll: HashSet<_> = l.chars().collect();
        let rr: HashSet<_> = r.chars().collect();
        score += ll.intersection(&rr).map(|c| priority(*c)).sum::<i64>()
    }
    score
}

fn part_two(input: &str) -> i64 {
    let mut score = 0;
    for group in input.lines().chunks(3).into_iter() {
        score += group
            .map(|l| l.chars().collect::<HashSet<_>>())
            .reduce(|acc, x| acc.intersection(&x).cloned().collect())
            .unwrap()
            .iter()
            .map(|c| priority(*c))
            .sum::<i64>();
    }
    score
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
