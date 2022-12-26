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
    input
        .lines()
        .map(|line| {
            let (l, r) = line.split_at(line.len() / 2);
            l.chars().find(|c| r.contains([*c])).map(priority).unwrap()
        })
        .sum()
}

fn part_two(input: &str) -> i64 {
    let mut score = 0;
    let mut ll = input.lines();
    while let (Some(a), Some(b), Some(c)) = (ll.next(), ll.next(), ll.next()) {
        score += a
            .chars()
            .find(|ch| b.contains([*ch]) && c.contains([*ch]))
            .map(|ch| priority(ch))
            .unwrap();
    }
    score
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[test]
fn test_03_p1() {
    assert_eq!(part_one(_TEST), 157);
    assert_eq!(part_one(_INPUT), 8105);
}

#[test]
fn test_03_p2() {
    assert_eq!(part_two(_TEST), 70);
    assert_eq!(part_two(_INPUT), 2363);
}
