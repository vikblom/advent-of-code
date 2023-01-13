use std::{cmp::Ordering, iter::Peekable};

const _INPUT: &str = include_str!("../../data/input_13.txt");

const _TEST: &str = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";

#[derive(Debug)]
enum Packet {
    Int(u32),
    List(Vec<Packet>),
}

impl Packet {
    fn from_str(input: &str) -> Packet {
        Packet::parse(&mut input.chars().peekable())
    }

    fn into_list(i: u32) -> Packet {
        Packet::List(vec![Packet::Int(i)])
    }

    fn parse(chars: &mut Peekable<std::str::Chars>) -> Packet {
        match chars.next() {
            Some(c) if '0' <= c && c <= '9' => {
                let next = *chars.peek().unwrap();
                if next == '0' {
                    chars.next(); // consume the peek
                    Packet::Int(10)
                } else {
                    Packet::Int(c.to_digit(10).unwrap())
                }
            }

            Some('[') => {
                let mut list = Vec::new();
                loop {
                    // Are we at the end of the list?
                    if let Some(&next) = chars.peek() {
                        if next == ']' {
                            chars.next(); // consume the peek
                            break;
                        }
                    }

                    list.push(Packet::parse(chars));

                    // Eat separator if its there.
                    if let Some(',') = chars.peek() {
                        chars.next(); // consume the peek
                    }
                }
                Packet::List(list)
            }
            _ => panic!("unexpected"),
        }
    }
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Packet::Int(l), Packet::Int(r)) => l.cmp(r),
            // Patch types if they are different.
            (Packet::Int(l), r) => Packet::into_list(*l).cmp(r),
            (l, Packet::Int(r)) => l.cmp(&Packet::into_list(*r)),
            // Both vectors,
            (Packet::List(l), Packet::List(r)) => {
                for (ll, rr) in l.iter().zip(r.iter()) {
                    match Packet::cmp(ll, rr) {
                        Ordering::Less => return Ordering::Less,
                        Ordering::Greater => return Ordering::Greater,
                        Ordering::Equal => {} // continue
                    }
                }
                // All elements ok, compare length
                l.len().cmp(&r.len())
            }
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Packet {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Packet {}

fn part_one(input: &str) -> usize {
    let mut score = 0;
    for (i, lr) in input.split("\n\n").enumerate() {
        if let Some((l, r)) = lr.split_once("\n") {
            let left = Packet::from_str(l);
            let right = Packet::from_str(r);
            // Pair indexed from 1.
            if left < right {
                score += i + 1;
            }
        }
    }
    score
}

fn part_two(input: &str) -> usize {
    let mark1 = Packet::from_str("[[2]]");
    let mark2 = Packet::from_str("[[6]]");

    let mut packs = vec![mark1, mark2];
    for line in input.split("\n") {
        if line == "" {
            continue;
        }
        packs.push(Packet::from_str(line));
    }
    packs.sort();

    let mark1 = Packet::from_str("[[2]]");
    let mark2 = Packet::from_str("[[6]]");
    packs
        .iter()
        .enumerate()
        .filter_map(|(i, p)| {
            if *p == mark1 || *p == mark2 {
                // Packets indexed from 1.
                Some(i + 1)
            } else {
                None
            }
        })
        .product()
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[test]
fn test_13_p1() {
    assert_eq!(part_one(_TEST), 13);
    assert_eq!(part_one(_INPUT), 4734);
}

#[test]
fn test_13_p2() {
    assert_eq!(part_two(_TEST), 140);
    assert_eq!(part_two(_INPUT), 21_836);
}
