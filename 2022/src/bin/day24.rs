use std::collections::{HashSet, VecDeque};

const _INPUT: &str = include_str!("../../data/input_24.txt");

const _TEST: &str = "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#";

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
enum Dir {
    North,
    South,
    West,
    East,
}

type Blizzard = (i64, i64, Dir);
type Storm = Vec<Blizzard>;

fn parse(input: &str) -> (Storm, (i64, i64), (i64, i64), i64, i64) {
    let mut storm = Vec::new();
    let mut start = None;
    let mut goal = (0, 0);
    let mut width = None;

    for (i, l) in input.lines().enumerate() {
        if width.is_none() {
            width = Some(l.len());
        }
        for (j, c) in l.as_bytes().iter().enumerate() {
            match *c {
                b'>' => storm.push((i as i64, j as i64, Dir::East)),
                b'v' => storm.push((i as i64, j as i64, Dir::South)),
                b'<' => storm.push((i as i64, j as i64, Dir::West)),
                b'^' => storm.push((i as i64, j as i64, Dir::North)),
                b'.' => {
                    if start.is_none() {
                        start = Some((i as i64, j as i64));
                    }
                    if goal.0 < i as i64 {
                        goal = (i as i64, j as i64)
                    }
                }
                _ => (),
            };
        }
    }
    // already including walls.
    let width = width.unwrap() as i64;
    // convert x index to n rows including walls.
    let height = goal.0 + 1;
    (storm, start.unwrap(), goal, height, width)
}

fn _draw(height: i64, width: i64, storm: &Storm) {
    for i in 0..height {
        for j in 0..width {
            if i == 0 || i == (height - 1) || j == 0 || j == (width - 1) {
                print!("#");
                continue;
            }

            let n = storm.iter().filter(|b| b.0 == i && b.1 == j).count();
            if n > 9 {
                print!("*");
            } else if n > 1 {
                print!("{}", n);
            } else {
                match storm.iter().find(|b| b.0 == i && b.1 == j) {
                    Some(s) => match s.2 {
                        Dir::East => print!(">"),
                        Dir::South => print!("v"),
                        Dir::West => print!("<"),
                        Dir::North => print!("^"),
                    },
                    None => print!("."),
                }
            }
        }
        println!()
    }
    println!()
}

// wrap n+add staying in [1, lim-1)
fn wrap(n: i64, add: i64, lim: i64) -> i64 {
    let n = n + add;
    if n == 0 {
        lim - 2
    } else if n == (lim - 1) {
        1
    } else {
        n
    }
}

fn progress(prev: &Storm, height: i64, width: i64) -> Storm {
    let mut next = Vec::new();

    for b in prev {
        let n = match b.2 {
            Dir::East => (b.0, wrap(b.1, 1, width), Dir::East),
            Dir::South => (wrap(b.0, 1, height), b.1, Dir::South),
            Dir::West => (b.0, wrap(b.1, -1, width), Dir::West),
            Dir::North => (wrap(b.0, -1, height), b.1, Dir::North),
        };

        next.push(n);
    }
    next
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
struct State {
    x: i64,
    y: i64,
    time: usize,
}

fn solve(
    t0: usize,
    start: (i64, i64),
    goal: (i64, i64),
    generations: &Vec<HashSet<(i64, i64)>>,
) -> usize {
    let mut seen = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(State {
        time: t0,
        x: start.0,
        y: start.1,
    });

    // BFS
    let mut best = generations.len() - 2;
    while let Some(at) = queue.pop_front() {
        if at.x == goal.0 && at.y == goal.1 {
            if at.time < best {
                best = at.time;
            }
        }

        if seen.contains(&at) {
            continue;
        }
        seen.insert(at.clone());

        if at.time > best {
            continue;
        }

        let free_next = &generations[at.time + 1];
        vec![
            (at.x + 1, at.y),
            (at.x - 1, at.y),
            (at.x, at.y + 1),
            (at.x, at.y - 1),
            (at.x, at.y),
        ]
        .iter()
        .for_each(|p| {
            if free_next.contains(p) || *p == start || *p == goal {
                queue.push_back(State {
                    time: at.time + 1,
                    x: p.0,
                    y: p.1,
                })
            }
        });
    }
    best
}

fn part_one(input: &str) -> i64 {
    let (mut storm, start, goal, height, width) = parse(input);

    // Make a vector with sets of open space at each t.
    let mut all_free = HashSet::new();
    for i in 1..(height - 1) {
        for j in 1..(width - 1) {
            all_free.insert((i, j));
        }
    }
    let mut generations = Vec::new();
    for _ in 0..1024 {
        let mut free_now = all_free.clone();
        for n in storm.iter() {
            free_now.remove(&(n.0, n.1));
        }
        generations.push(free_now);

        storm = progress(&storm, height, width);
    }

    solve(0, start, goal, &generations) as i64
}

fn part_two(input: &str) -> i64 {
    let (mut storm, start, goal, height, width) = parse(input);

    // Make a vector with sets of open space at each t.
    let mut all_free = HashSet::new();
    for i in 1..(height - 1) {
        for j in 1..(width - 1) {
            all_free.insert((i, j));
        }
    }
    let mut generations = Vec::new();
    for _ in 0..1024 {
        let mut free_now = all_free.clone();
        for n in storm.iter() {
            free_now.remove(&(n.0, n.1));
        }
        generations.push(free_now);

        storm = progress(&storm, height, width);
    }

    let t1 = solve(0, start, goal, &generations);
    let t2 = solve(t1, goal, start, &generations);
    let t3 = solve(t2, start, goal, &generations);

    t3 as i64
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        assert_eq!(part_one(_INPUT), 297);
    }
}
