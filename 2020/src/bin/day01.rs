use itertools::Itertools;

const _INPUT: &str = include_str!("../../data/input_01.txt");

const _TEST: &str = "1721
979
366
299
675
1456";

fn part_one(input: &str) -> usize {
    input
        .lines()
        .map(|i| i.parse::<usize>().unwrap())
        .combinations(2)
        .filter(|v| v[0] + v[1] == 2020)
        .next()
        .map(|v| v[0] * v[1])
        .unwrap()
}

fn part_two(input: &str) -> usize {
    input
        .lines()
        .map(|i| i.parse::<usize>().unwrap())
        .combinations(3)
        .filter(|v| v.iter().sum::<usize>() == 2020)
        .next()
        .map(|v| v.iter().product())
        .unwrap()
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
