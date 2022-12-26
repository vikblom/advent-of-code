use std::collections::HashSet;

const _INPUT: &str = include_str!("../../data/input_06.txt");

const _TEST: &str = "bvwbjplbgvbhsrlpgdmjqwftvncz";

fn part_one(input: &str) -> usize {
    input
        .as_bytes()
        .windows(4)
        .position(|v| HashSet::<_>::from_iter(v).len() == 4)
        .unwrap()
        + 4
}

fn part_two(input: &str) -> usize {
    input
        .as_bytes()
        .windows(14)
        .position(|v| HashSet::<_>::from_iter(v).len() == 14)
        .unwrap()
        + 14
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[test]
fn test_06_p1() {
    assert_eq!(part_one(_TEST), 5);
    assert_eq!(part_one(_INPUT), 1929);
}

#[test]
fn test_06_p2() {
    assert_eq!(part_two(_TEST), 23);
    assert_eq!(part_two(_INPUT), 3298);
}
