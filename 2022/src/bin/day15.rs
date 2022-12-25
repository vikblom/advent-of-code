use std::collections::HashSet;

use regex::Regex;

const _INPUT: &str = include_str!("../../data/input_15.txt");

// const _TEST: &str = include_str!("../../data/test_15.txt");

fn part_one(input: &str, row: i32) -> i64 {
    let re = Regex::new(
        r"^Sensor at x=(\-?\d+), y=(\-?\d+): closest beacon is at x=(\-?\d+), y=(\-?\d+)",
    )
    .unwrap();

    let mut deadzone = HashSet::new();
    let mut busy = HashSet::new();

    for l in input.lines() {
        let capt = re.captures(l).unwrap();
        // x -> right
        // y -> down
        let sx = capt[1].parse::<i32>().unwrap();
        let sy = capt[2].parse::<i32>().unwrap();
        let bx = capt[3].parse::<i32>().unwrap();
        let by = capt[4].parse::<i32>().unwrap();
        // println!("{} {} {} {}", sx, sy, bx, by);

        if by == row {
            busy.insert((by, bx));
        }

        // manhattan distance reach of the "dead zone"
        let distance = (bx - sx).abs() + (by - sy).abs();
        // The dead zone will the thinner further up/down from the sensor.
        // How far away is the line we are interested in.
        let reach = (row - sy).abs();

        let width = distance - reach;

        for i in (sx - width)..=(sx + width) {
            deadzone.insert(i);
        }
    }

    (deadzone.len() - busy.len()) as i64
}

struct Pair {
    sy: i32,
    sx: i32,
    reach: i32,
}

fn part_two(input: &str, n: i32) -> i64 {
    let re = Regex::new(
        r"^Sensor at x=(\-?\d+), y=(\-?\d+): closest beacon is at x=(\-?\d+), y=(\-?\d+)",
    )
    .unwrap();

    let mut pairs = Vec::new();

    for l in input.lines() {
        let capt = re.captures(l).unwrap();
        // x -> right
        // y -> down
        let sx = capt[1].parse::<i32>().unwrap();
        let sy = capt[2].parse::<i32>().unwrap();
        let bx = capt[3].parse::<i32>().unwrap();
        let by = capt[4].parse::<i32>().unwrap();
        let reach = (bx - sx).abs() + (by - sy).abs();
        // println!("{} {} {} {}", sx, sy, bx, by);

        pairs.push(Pair {
            sy,
            sx,
            // bx,
            // by,
            reach,
        });
    }

    // This order never changes.
    pairs.sort_by(|a, b| a.sx.cmp(&b.sx));

    for row in 0..=n {
        // Each (not empty) range that covers needle will push it forward, outside that range.
        let mut needle = 0;
        pairs
            .iter()
            .filter_map(|p| {
                let width = p.reach - (row - p.sy).abs();
                if width > 0 {
                    Some((p.sx - width, p.sx + width))
                } else {
                    None
                }
            })
            .for_each(|p| {
                if p.0 <= needle && needle <= p.1 {
                    needle = p.1 + 1;
                }
            });
        if needle <= n {
            return (row as i64) + (needle as i64) * 4_000_000;
        }
    }

    panic!("no match");
}

fn main() {
    // println!("part 1 test: {}", part_one(_TEST, 10));
    println!("part 1 input: {}", part_one(_INPUT, 2_000_000));

    // println!("part 2 test: {}", part_two(_TEST, 20));
    println!("part 2 input: {}", part_two(_INPUT, 4_000_000));
}
