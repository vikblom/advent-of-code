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

fn descend(chars: &mut Peekable<std::str::Chars>) -> Packet {
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
                // Could be empty list.
                if let Some(&next) = chars.peek() {
                    if next == ']' {
                        chars.next(); // consume the peek
                        break;
                    }
                }
                list.push(descend(chars));
                let then = chars.next().unwrap();
                if then == ']' {
                    break;
                }
            }
            Packet::List(list)
        }
        _ => panic!("unexpected"),
    }
}

fn parse(input: &str) -> Packet {
    descend(&mut input.chars().peekable())
}

// CORRECT: left less than right
fn ordered(left: &Packet, right: &Packet) -> Ordering {
    match (left, right) {
        (Packet::Int(l), Packet::Int(r)) => l.cmp(r),
        // Patch types if they are different.
        (Packet::Int(l), r) => ordered(&Packet::List(vec![Packet::Int(*l)]), r),
        (l, Packet::Int(r)) => ordered(l, &Packet::List(vec![Packet::Int(*r)])),
        // Both vectors,
        (Packet::List(l), Packet::List(r)) => {
            for (ll, rr) in l.iter().zip(r.iter()) {
                match ordered(ll, rr) {
                    Ordering::Less => return Ordering::Less,
                    Ordering::Greater => return Ordering::Greater,
                    _ => {} // continue
                }
            }
            // All elements ok, compare length
            l.len().cmp(&r.len())
        }
    }
}

fn part_one(input: &str) -> usize {
    let mut score = 0;
    for (i, lr) in input.split("\n\n").enumerate() {
        if let Some((l, r)) = lr.split_once("\n") {
            let left = parse(l);
            let right = parse(r);

            // Pair indexed from 1.
            if ordered(&left, &right) == Ordering::Less {
                score += i + 1;
            }
        }
    }

    score
}

fn part_two(input: &str) -> usize {
    let mark1 = parse("[[2]]");
    let mark2 = parse("[[6]]");

    let mut packs = vec![mark1, mark2];
    for line in input.split("\n") {
        if line == "" {
            continue;
        }
        packs.push(parse(line));
    }
    packs.sort_by(|a, b| ordered(a, b));

    let mark1 = parse("[[2]]");
    let mark2 = parse("[[6]]");
    packs
        .iter()
        .enumerate()
        .filter_map(|(i, p)| {
            if ordered(p, &mark1) == Ordering::Equal || ordered(p, &mark2) == Ordering::Equal {
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
