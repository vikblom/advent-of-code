use std::collections::{HashMap, HashSet};

const _INPUT: &str = include_str!("../../data/input_23.txt");

const _TEST: &str = "..............
..............
.......#......
.....###.#....
...#...#.#....
....#...##....
...#.###......
...##.#.##....
....#..#......
..............
..............
..............";

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Dir {
    North,
    South,
    West,
    East,
}

type Pos = (i64, i64);
type Elves = HashSet<Pos>;

fn parse(input: &str) -> Elves {
    let mut elves = HashSet::new();

    for (i, l) in input.lines().enumerate() {
        for (j, c) in l.as_bytes().iter().enumerate() {
            match *c {
                b'#' => elves.insert((i as i64, j as i64)),
                _ => false,
            };
        }
    }
    elves
}

fn solo(elves: &Elves, pos: &Pos) -> bool {
    vec![
        // From top left, row by row.
        (pos.0 - 1, pos.1 - 1),
        (pos.0 - 1, pos.1),
        (pos.0 - 1, pos.1 + 1),
        (pos.0, pos.1 - 1),
        (pos.0, pos.1 + 1),
        (pos.0 + 1, pos.1 - 1),
        (pos.0 + 1, pos.1),
        (pos.0 + 1, pos.1 + 1),
    ]
    .iter()
    .all(|p| !elves.contains(p))
}

fn check(p: Pos, d: Dir) -> Vec<Pos> {
    match d {
        Dir::North => vec![(p.0 - 1, p.1 - 1), (p.0 - 1, p.1), (p.0 - 1, p.1 + 1)],
        Dir::South => vec![(p.0 + 1, p.1 - 1), (p.0 + 1, p.1), (p.0 + 1, p.1 + 1)],
        Dir::West => vec![(p.0 - 1, p.1 - 1), (p.0, p.1 - 1), (p.0 + 1, p.1 - 1)],
        Dir::East => vec![(p.0 - 1, p.1 + 1), (p.0, p.1 + 1), (p.0 + 1, p.1 + 1)],
    }
}

fn next(p: Pos, d: Dir) -> Pos {
    match d {
        Dir::North => (p.0 - 1, p.1),
        Dir::South => (p.0 + 1, p.1),
        Dir::West => (p.0, p.1 - 1),
        Dir::East => (p.0, p.1 + 1),
    }
}

fn part_one(input: &str) -> i64 {
    let mut elves = parse(input);

    let mut directions = vec![Dir::North, Dir::South, Dir::West, Dir::East];

    let mut i = 0;
    loop {
        i += 1;

        if elves.iter().all(|p| solo(&elves, p)) {
            break;
        }

        // no nbr -> stand still
        // else plan for first dir w/o nbrs
        let mut to_move = HashMap::new(); // new - old
        let mut collided = HashSet::new(); // new that had multiple asks.
        for e in elves.iter() {
            if solo(&elves, &e) {
                continue;
            }

            for &d in directions.iter() {
                if check(*e, d).iter().all(|&v| !elves.contains(&v)) {
                    let n = next(*e, d);
                    if !to_move.contains_key(&n) {
                        to_move.insert(n, e);
                    } else {
                        collided.insert(n);
                    }
                    break;
                }
            }
        }

        let mut new = elves.clone();
        // only move according to plan if single one to plan that
        for (to, from) in to_move.iter() {
            if collided.contains(to) {
                continue;
            }
            new.remove(from);
            new.insert(*to);
        }
        elves = new;

        // rotate order of considering directions.
        directions.rotate_left(1);

        if i == 10 {
            break;
        }
    }

    let max_row = elves.iter().map(|e| e.0).max().unwrap();
    let max_col = elves.iter().map(|e| e.1).max().unwrap();
    let min_row = elves.iter().map(|e| e.0).min().unwrap();
    let min_col = elves.iter().map(|e| e.1).min().unwrap();

    (max_row - min_row + 1) * (max_col - min_col + 1) - elves.len() as i64
}

fn _draw(elves: &Elves) {
    let max_row = elves.iter().map(|e| e.0).max().unwrap();
    let max_col = elves.iter().map(|e| e.1).max().unwrap();
    let min_row = elves.iter().map(|e| e.0).min().unwrap();
    let min_col = elves.iter().map(|e| e.1).min().unwrap();

    for i in min_row..=max_row {
        for j in min_col..=max_col {
            match elves.get(&(i, j)) {
                Some(_) => {
                    print!("#")
                }
                None => print!("."),
            }
        }
        println!()
    }
    println!()
}

fn part_two(input: &str) -> i64 {
    let mut elves = parse(input);
    let mut directions = vec![Dir::North, Dir::South, Dir::West, Dir::East];

    let mut i = 0;
    loop {
        i += 1;

        if elves.iter().all(|p| solo(&elves, p)) {
            break;
        }

        let mut to_move = HashMap::new(); // new -- old
        let mut collided = HashSet::new(); // new that had multiple asks.
        for e in elves.iter() {
            if solo(&elves, &e) {
                continue;
            }

            for &d in directions.iter() {
                if check(*e, d).iter().all(|&v| !elves.contains(&v)) {
                    let n = next(*e, d);
                    if !to_move.contains_key(&n) {
                        to_move.insert(n, e);
                    } else {
                        collided.insert(n);
                    }
                    break;
                }
            }
        }

        let mut moved = elves.clone();
        // only move according to plan if single one to plan that
        for (to, from) in to_move.iter() {
            if collided.contains(to) {
                continue;
            }
            moved.remove(from);
            moved.insert(*to);
        }
        elves = moved;

        // rotate order of considering directions.
        directions.rotate_left(1);
    }

    i
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
