use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};

const _INPUT: &str = include_str!("../../data/input_12.txt");

const _TEST: &str = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";

// State is a part in the search.
// Implements some traits to play nice with BinaryHeap.
#[derive(Debug)]
struct State {
    loc: (i64, i64),
    dist: u32,
}

impl Eq for State {}

// Smallest dist first.
impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.dist.eq(&other.dist)
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.dist.cmp(&self.dist)
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn solve(map: &HashMap<(i64, i64), u8>, start: (i64, i64), end: (i64, i64)) -> Option<u32> {
    let mut seen: HashSet<(i64, i64)> = HashSet::new();
    let mut next: BinaryHeap<State> = BinaryHeap::new();
    next.push(State {
        loc: start,
        dist: 0,
    });
    seen.insert(start);

    while let Some(at) = next.pop() {
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
                next.push(State {
                    loc: *tmp.0,
                    dist: at.dist + 1,
                })
            }
        });
    }
    None
}

fn part_one(input: &str) -> i64 {
    let mut map: HashMap<(i64, i64), u8> = HashMap::new();
    let mut start: (i64, i64) = (0, 0);
    let mut end: (i64, i64) = (0, 0);

    for (i, l) in input.lines().enumerate() {
        for (j, c) in l.as_bytes().iter().enumerate() {
            if *c == b'S' {
                start = (i as i64, j as i64);
                map.insert((i as i64, j as i64), b'a');
            } else if *c == b'E' {
                end = (i as i64, j as i64);
                map.insert((i as i64, j as i64), b'z');
            } else {
                map.insert((i as i64, j as i64), *c);
            }
        }
    }

    solve(&map, start, end).unwrap() as i64
}

fn part_two(input: &str) -> i64 {
    let mut map: HashMap<(i64, i64), u8> = HashMap::new();
    let mut end: (i64, i64) = (0, 0);

    for (i, l) in input.lines().enumerate() {
        for (j, c) in l.as_bytes().iter().enumerate() {
            if *c == b'S' {
                map.insert((i as i64, j as i64), b'a');
            } else if *c == b'E' {
                end = (i as i64, j as i64);
                map.insert((i as i64, j as i64), b'z');
            } else {
                map.insert((i as i64, j as i64), *c);
            }
        }
    }

    map.iter()
        .filter(|&(_, height)| *height == b'a')
        .filter_map(|(loc, _)| solve(&map, *loc, end))
        .min()
        .unwrap() as i64
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
