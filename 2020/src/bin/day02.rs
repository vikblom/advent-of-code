use std::fmt::Debug;

use regex::Regex;

const _INPUT: &str = include_str!("../../data/input_02.txt");

const _TEST: &str = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
";

fn part_one(input: &str) -> usize {
    let re = Regex::new(r"^(\d+)-(\d+) (\w): (\w+)$").unwrap();
    input
        .lines()
        .map(|l| re.captures(l).unwrap())
        .filter(|c| {
            let num = c[4].matches(&c[3]).count();
            num >= c[1].parse().unwrap() && num <= c[2].parse().unwrap()
        })
        .count()
    // .map(|v| println!("{:?}", v))
    // .for_each(drop);
}

fn part_two(input: &str) -> usize {
    let re = Regex::new(r"^(\d+)-(\d+) (\w): (\w+)$").unwrap();
    input
        .lines()
        .map(|l| re.captures(l).unwrap())
        .filter(|c| {
            (c[4].chars().nth(c[1].parse::<usize>().unwrap() - 1) == c[3].chars().next())
                ^ (c[4].chars().nth(c[2].parse::<usize>().unwrap() - 1) == c[3].chars().next())
            // let num = c[4].matches(&c[3]).count();
            // num >= c[1].parse().unwrap() && num <= c[2].parse().unwrap()
        })
        .count()
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
