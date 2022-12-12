use std::collections::{HashMap, HashSet, VecDeque};

const _INPUT: &str = include_str!("../../data/input_12.txt");

const _TEST: &str = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";

// Node represents one state in the state space.
#[derive(Debug)]
struct Node {
    loc: (u16, u16),
    dist: u32,
}

fn solve(map: &HashMap<(u16, u16), u8>, start: (u16, u16), end: (u16, u16)) -> Option<u32> {
    let mut seen: HashSet<(u16, u16)> = HashSet::new();
    let mut next = VecDeque::new();
    next.push_back(Node {
        loc: start,
        dist: 0,
    });
    seen.insert(start);

    while let Some(at) = next.pop_front() {
        if at.loc == end {
            return Some(at.dist);
        }
        // For each nbr
        vec![
            (at.loc.0 - 1, at.loc.1),
            (at.loc.0 + 1, at.loc.1),
            (at.loc.0, at.loc.1 - 1),
            (at.loc.0, at.loc.1 + 1),
        ]
        .iter()
        .filter_map(|pos| match map.get(pos) {
            Some(h) if *h <= map.get(&at.loc).unwrap() + 1 => Some((pos, h)),
            _ => None,
        })
        .for_each(|tmp| {
            if !seen.contains(tmp.0) {
                seen.insert(*tmp.0);
                next.push_back(Node {
                    loc: *tmp.0,
                    dist: at.dist + 1,
                })
            }
        });
    }
    None
}

fn part_one(input: &str) -> u16 {
    let mut map: HashMap<(u16, u16), u8> = HashMap::new();
    let mut start: (u16, u16) = (0, 0);
    let mut end: (u16, u16) = (0, 0);

    for (i, l) in input.lines().enumerate() {
        for (j, c) in l.as_bytes().iter().enumerate() {
            if *c == b'S' {
                start = (i as u16, j as u16);
                map.insert((i as u16, j as u16), b'a');
            } else if *c == b'E' {
                end = (i as u16, j as u16);
                map.insert((i as u16, j as u16), b'z');
            } else {
                map.insert((i as u16, j as u16), *c);
            }
        }
    }

    solve(&map, start, end).unwrap() as u16
}

fn part_two(input: &str) -> u16 {
    let mut map: HashMap<(u16, u16), u8> = HashMap::new();
    let mut end: (u16, u16) = (0, 0);

    for (i, l) in input.lines().enumerate() {
        for (j, c) in l.as_bytes().iter().enumerate() {
            if *c == b'S' {
                map.insert((i as u16, j as u16), b'a');
            } else if *c == b'E' {
                end = (i as u16, j as u16);
                map.insert((i as u16, j as u16), b'z');
            } else {
                map.insert((i as u16, j as u16), *c);
            }
        }
    }

    map.iter()
        .filter(|&(_, height)| *height == b'a')
        .filter_map(|(loc, _)| solve(&map, *loc, end))
        .min()
        .unwrap() as u16
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
