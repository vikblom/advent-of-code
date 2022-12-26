use itertools::Itertools;

const _INPUT: &str = include_str!("../../data/input_01.txt");

const _TEST: &str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

fn part_one(input: &str) -> usize {
    input
        .split("\n\n")
        .map(|elf| elf.lines().map(|l| l.parse::<usize>().unwrap()).sum())
        .max()
        .unwrap()
}

fn part_two(input: &str) -> usize {
    input
        .split("\n\n")
        .map(|elf| elf.lines().map(|l| l.parse::<usize>().unwrap()).sum())
        .sorted_by(|a: &usize, b: &usize| b.cmp(a))
        .take(3)
        .sum()
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[test]
fn test_01_p1() {
    assert_eq!(part_one(_TEST), 24_000);
    assert_eq!(part_one(_INPUT), 71_780);
}

#[test]
fn test_01_p2() {
    assert_eq!(part_two(_TEST), 45_000);
    assert_eq!(part_two(_INPUT), 212_489);
}
