use std::collections::HashMap;

const _INPUT: &str = include_str!("../../data/input_14.txt");

const _TEST: &str = "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";

fn _draw(map: &HashMap<(i32, i32), u8>) {
    let depth = map
        .iter()
        .filter_map(|(k, &v)| if v == b'#' { Some(k.1) } else { None })
        .max()
        .unwrap();

    let left = map.keys().map(|k| k.0).min().unwrap();
    let right = map.keys().map(|k| k.0).max().unwrap();

    for j in 0..(depth + 3) {
        for i in (left - 2)..(right + 2) {
            if let Some(b) = map.get(&(i, j)) {
                print!("{}", *b as char);
            } else if j == (depth + 2) {
                print!("#")
            } else {
                print!(".")
            }
        }
        println!("");
    }
    println!("\n");
}

fn parse(input: &str) -> HashMap<(i32, i32), u8> {
    let mut map = HashMap::new();
    for l in input.lines() {
        let corners = l
            .split(" -> ")
            .map(|pair| {
                let (a, b) = pair.split_once(",").unwrap();
                (a.parse::<i32>().unwrap(), b.parse::<i32>().unwrap())
            })
            .collect::<Vec<_>>();
        for w in corners.windows(2) {
            let diff = (w[1].0 - w[0].0, w[1].1 - w[0].1);
            let step = (diff.0.signum(), diff.1.signum());

            let mut at = w[0];
            while at != w[1] {
                map.insert(at, b'#');
                at = (at.0 + step.0, at.1 + step.1);
            }
            map.insert(w[1], b'#');
        }
    }
    map
}

fn part_one(input: &str) -> i64 {
    let mut map = parse(input);
    let max = map.keys().map(|k| k.1).max().unwrap();

    // i = 'o' placed before current loop.
    for i in 0.. {
        // draw(&map);
        let mut sand = (500, 0);
        loop {
            if sand.1 >= max {
                return i; // falling out
            }
            if let Some(p) = vec![
                (sand.0, sand.1 + 1),
                (sand.0 - 1, sand.1 + 1),
                (sand.0 + 1, sand.1 + 1),
            ]
            .iter()
            .filter(|p| !map.contains_key(p))
            .next()
            {
                sand = *p;
            } else {
                map.insert(sand, b'o');
                break;
            }
        }
        if sand == (500, 0) {
            // backed up
            return i;
        }
    }
    0
}

fn part_two(input: &str) -> i64 {
    let mut map = parse(input);

    let floor = 2 + map.keys().map(|k| k.1).max().unwrap();
    // i = 'o' placed counting current loop.
    for i in 1.. {
        // draw(&map);
        let mut sand = (500, 0);
        loop {
            if let Some(p) = vec![
                (sand.0, sand.1 + 1),
                (sand.0 - 1, sand.1 + 1),
                (sand.0 + 1, sand.1 + 1),
            ]
            .iter()
            .find(|p| !map.contains_key(p))
            {
                sand = *p;
                if sand.1 == (floor - 1) {
                    map.insert(sand, b'o');
                    break;
                }
            } else {
                map.insert(sand, b'o');
                break;
            }
        }
        if sand == (500, 0) {
            // backed up
            return i;
        }
    }
    0
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
