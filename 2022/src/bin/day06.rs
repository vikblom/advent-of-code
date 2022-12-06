use std::collections::HashSet;

const _INPUT: &str = include_str!("../../data/input_06.txt");

const _TEST: &str = "bvwbjplbgvbhsrlpgdmjqwftvncz";

fn part_one(input: &str) -> usize {
    for (i, v) in input.chars().collect::<Vec<_>>().windows(4).enumerate() {
        let set: HashSet<_> = v.into_iter().collect();
        if set.len() == 4 {
            return 4 + i;
        };
    }
    0
}

fn part_two(input: &str) -> usize {
    for (i, v) in input.chars().collect::<Vec<_>>().windows(14).enumerate() {
        let set: HashSet<_> = v.into_iter().collect();
        if set.len() == 14 {
            return 14 + i;
        };
    }
    0
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
