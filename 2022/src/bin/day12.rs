use std::collections::{HashMap, HashSet, VecDeque};

const _INPUT: &str = include_str!("../../data/input_12.txt");

const _TEST: &str = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";

// Node represents one state in the state space.
// Optimization: Fit in 8 bytes.
#[derive(Debug)]
struct Node {
    loc: (i32, i32),
    dist: u32,
}

fn parse(input: &str) -> (HashMap<(i32, i32), u8>, (i32, i32), (i32, i32)) {
    let mut map = HashMap::new();
    let mut start = (0, 0);
    let mut end = (0, 0);

    for (i, l) in input.lines().enumerate() {
        for (j, c) in l.as_bytes().iter().enumerate() {
            if *c == b'S' {
                start = (i as i32, j as i32);
                map.insert((i as i32, j as i32), b'a');
            } else if *c == b'E' {
                end = (i as i32, j as i32);
                map.insert((i as i32, j as i32), b'z');
            } else {
                map.insert((i as i32, j as i32), *c);
            }
        }
    }
    (map, start, end)
}

fn part_one(input: &str) -> Option<u32> {
    let (map, start, end) = parse(input);

    let mut seen: HashSet<(i32, i32)> = HashSet::new();
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

fn part_two(input: &str) -> Option<u32> {
    let (map, _, end) = parse(input);

    // Flip the search, begin from end and spread out how many moves it takes
    // to reach each nbr. The first 'a' reached is the closest one.
    let mut seen: HashMap<(i32, i32), u32> = HashMap::new();
    let mut next = VecDeque::new();
    next.push_back(Node { loc: end, dist: 0 });
    seen.insert(end, 0);

    while let Some(at) = next.pop_front() {
        if *map.get(&at.loc).unwrap() == b'a' {
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
            // Moving backwards, allow at.loc height-1 or higher.
            Some(h) if *h >= map.get(&at.loc).unwrap() - 1 => Some((pos, h)),
            _ => None,
        })
        .for_each(|tmp| {
            if !seen.contains_key(tmp.0) {
                seen.insert(*tmp.0, at.dist + 1);
                next.push_back(Node {
                    loc: *tmp.0,
                    dist: at.dist + 1,
                })
            }
        });
    }
    None
}

fn main() {
    println!("part 1 test: {:?}", part_one(_TEST));
    println!("part 1 input: {:?}", part_one(_INPUT));

    println!("part 2 test: {:?}", part_two(_TEST));
    println!("part 2 input: {:?}", part_two(_INPUT));
}

#[test]
fn test_12_p1() {
    assert_eq!(part_one(_TEST), Some(31));
    assert_eq!(part_one(_INPUT), Some(352));
}

#[test]
fn test_12_p2() {
    assert_eq!(part_two(_TEST), Some(29));
    assert_eq!(part_two(_INPUT), Some(345));
}
