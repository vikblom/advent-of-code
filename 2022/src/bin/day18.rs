use std::cmp;
use std::collections::{HashSet, VecDeque};

use itertools::Itertools;

const _INPUT: &str = include_str!("../../data/input_18.txt");

const _TEST: &str = "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5";

fn nbrs(d: (i64, i64, i64)) -> Vec<(i64, i64, i64)> {
    vec![
        (d.0 + 1, d.1, d.2),
        (d.0 - 1, d.1, d.2),
        (d.0, d.1 + 1, d.2),
        (d.0, d.1 - 1, d.2),
        (d.0, d.1, d.2 + 1),
        (d.0, d.1, d.2 - 1),
    ]
}

fn part_one(input: &str) -> i64 {
    let mut droplets = HashSet::new();

    let mut total = 0;
    let mut blocked = 0;

    for l in input.lines() {
        let (a, b, c) = l
            .split(",")
            .map(|d| d.parse::<i64>().unwrap())
            .collect_tuple()
            .unwrap();
        // println!("{} {} {}", a, b, c);

        for nbr in nbrs((a, b, c)) {
            if droplets.contains(&nbr) {
                // Blocks one side from each of the drops.
                blocked += 2;
            }
        }

        droplets.insert((a, b, c));
        total += 6;
    }

    total - blocked
}

fn inbounds(xyz: (i64, i64, i64), min: (i64, i64, i64), max: (i64, i64, i64)) -> bool {
    min.0 <= xyz.0
        && min.1 <= xyz.1
        && min.2 <= xyz.2
        && xyz.0 <= max.0
        && xyz.1 <= max.1
        && xyz.2 <= max.2
}

fn part_two(input: &str) -> i64 {
    let mut total = 0;
    let mut blocked = 0;

    let mut droplets = HashSet::new();
    for l in input.lines() {
        let (a, b, c) = l
            .split(",")
            .map(|d| d.parse::<i64>().unwrap())
            .collect_tuple()
            .unwrap();
        // println!("{} {} {}", a, b, c);

        for nbr in nbrs((a, b, c)) {
            if droplets.contains(&nbr) {
                // Blocks one side from each of the drops.
                blocked += 2;
            }
        }

        droplets.insert((a, b, c));
        total += 6;
    }

    // Add a margin to get around droplets on the edges.
    let m = 1;
    let min = (-m, -m, -m);
    let max = droplets.iter().fold((0, 0, 0), |acc, v| {
        (
            cmp::max(acc.0, v.0 + m),
            cmp::max(acc.1, v.1 + m),
            cmp::max(acc.2, v.2 + m),
        )
    });

    // Find where the water can reach.
    let mut reachable = HashSet::new();
    let mut next = VecDeque::new();
    // Start from the margin.
    next.push_front(min);
    while let Some(xyz) = next.pop_back() {
        reachable.insert(xyz);
        for nbr in nbrs(xyz) {
            if inbounds(xyz, min, max) && !reachable.contains(&nbr) && !droplets.contains(&nbr) {
                next.push_back(nbr);
            }
        }
    }

    // Each non-reachable neighbour of a droplet is one less surface for cooling.
    let mut air = 0;
    for d in &droplets {
        for nbr in nbrs(*d) {
            if !(droplets.contains(&nbr) || reachable.contains(&nbr)) {
                air += 1;
            }
        }
    }

    total - blocked - air
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[test]
fn test_18_p1() {
    assert_eq!(part_one(_TEST), 64);
    assert_eq!(part_one(_INPUT), 4300);
}

#[test]
fn test_18_p2() {
    assert_eq!(part_two(_TEST), 58);
    assert_eq!(part_two(_INPUT), 2490);
}
