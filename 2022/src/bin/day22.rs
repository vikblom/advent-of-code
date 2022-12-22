use std::collections::HashMap;

const _INPUT: &str = include_str!("../../data/input_22.txt");

#[derive(Debug)]
enum Instr {
    Right,
    Left,
    Move(i64),
}

#[derive(Debug, Clone, Eq, PartialEq)]
enum Dir {
    East,
    West,
    North,
    South,
}

fn rotate(facing: Dir, turn: Instr) -> Dir {
    match turn {
        Instr::Right => match facing {
            Dir::East => Dir::South,
            Dir::South => Dir::West,
            Dir::West => Dir::North,
            Dir::North => Dir::East,
        },
        Instr::Left => match facing {
            Dir::East => Dir::North,
            Dir::South => Dir::East,
            Dir::West => Dir::South,
            Dir::North => Dir::West,
        },
        _ => panic!("invalid input"),
    }
}

fn delta(facing: &Dir) -> (i64, i64) {
    match facing {
        Dir::East => (0, 1),
        Dir::South => (1, 0),
        Dir::West => (0, -1),
        Dir::North => (-1, 0),
    }
}

fn score(facing: &Dir) -> i64 {
    match facing {
        Dir::East => 0,
        Dir::South => 1,
        Dir::West => 2,
        Dir::North => 3,
    }
}

fn next_wrapping(map: &HashMap<(i64, i64), bool>, pos: &(i64, i64, Dir)) -> (i64, i64) {
    let dir = delta(&pos.2);
    let mut next = (pos.0 + dir.0, pos.1 + dir.1);
    if !map.contains_key(&next) {
        // Keep going until the one after next is outside.
        let reverse = (-1 * dir.0, -1 * dir.1);
        while let Some(_) = map.get(&(next.0 + reverse.0, next.1 + reverse.1)) {
            next = (next.0 + reverse.0, next.1 + reverse.1);
        }
    }

    next
}

fn parse(input: &str) -> (HashMap<(i64, i64), bool>, Vec<Instr>, (i64, i64, Dir)) {
    let (map_raw, moves_raw) = input.split_once("\n\n").unwrap();

    let mut map = HashMap::new();
    let mut pos = (0, 0, Dir::East);

    for (i, l) in map_raw.lines().enumerate() {
        for (j, c) in l.as_bytes().iter().enumerate() {
            match *c {
                b'.' => {
                    if pos == (0, 0, Dir::East) {
                        pos = (i as i64, j as i64, Dir::East)
                    }
                    map.insert((i as i64, j as i64), true)
                }
                b'#' => map.insert((i as i64, j as i64), false),
                _ => None,
            };
        }
    }

    let mut moves = Vec::new();
    let moves_raw = &moves_raw.replace("R", " R ");
    let moves_raw = moves_raw.replace("L", " L ");
    for m in moves_raw.trim().split(" ") {
        moves.push(match m {
            "R" => Instr::Right,
            "L" => Instr::Left,
            n => Instr::Move(n.parse::<i64>().unwrap()),
        });
    }

    (map, moves, pos)
}

fn part_one(input: &str) -> i64 {
    let (map, moves, mut pos) = parse(input);

    for mov in moves {
        pos = match mov {
            Instr::Right => (pos.0, pos.1, rotate(pos.2, Instr::Right)),
            Instr::Left => (pos.0, pos.1, rotate(pos.2, Instr::Left)),
            Instr::Move(n) => {
                for _ in 0..n {
                    let next = next_wrapping(&map, &pos);
                    match map.get(&next) {
                        Some(true) => {
                            // Open space.
                            pos = (next.0, next.1, pos.2);
                        }
                        Some(false) => {
                            // Wall.
                            break;
                        }
                        // Should always get something in the map.
                        _ => panic!("unreachable"),
                    }
                }
                //

                pos
            }
        };
    }

    1000 * (pos.0 + 1) + 4 * (pos.1 + 1) + score(&pos.2)
}

fn next_cube_wrapping(map: &HashMap<(i64, i64), bool>, pos: &(i64, i64, Dir)) -> (i64, i64, Dir) {
    let dir = delta(&pos.2);
    let mut next = (pos.0 + dir.0, pos.1 + dir.1, pos.2.clone());
    if !map.contains_key(&(next.0, next.1)) {
        // Figure out what side we are leaving, for far along that edge we are.
        // Match arms finds the edge to move to, tweaking distance and direction.
        // Note: Logic based on pos, place before we drop of the edge.
        match pos.2 {
            Dir::East => {
                let dist = pos.0 % 50;
                if pos.0 < 50 {
                    // c - f
                    next = (149 - dist, 99, Dir::West)
                } else if pos.0 < 100 {
                    // e - i
                    next = (49, 100 + dist, Dir::North)
                } else if pos.0 < 150 {
                    // i - l
                    next = (49 - dist, 149, Dir::West)
                } else if pos.0 < 200 {
                    // k - n
                    next = (149, 50 + dist, Dir::North)
                } else {
                    panic!("unreachable")
                }
            }
            Dir::South => {
                let dist = pos.1 % 50;
                if pos.1 < 50 {
                    next = (0, 100 + dist, Dir::South)
                } else if pos.1 < 100 {
                    next = (150 + dist, 49, Dir::West)
                } else if pos.1 < 150 {
                    next = (50 + dist, 99, Dir::West)
                } else {
                    panic!("unreachable")
                }
            }
            Dir::West => {
                let dist = pos.0 % 50;
                if pos.0 < 50 {
                    next = (149 - dist, 0, Dir::East)
                } else if pos.0 < 100 {
                    next = (100, 0 + dist, Dir::South)
                } else if pos.0 < 150 {
                    next = (49 - dist, 50, Dir::East)
                } else if pos.0 < 200 {
                    next = (0, 50 + dist, Dir::South)
                } else {
                    panic!("unreachable")
                }
            }
            Dir::North => {
                let dist = pos.1 % 50;
                if pos.1 < 50 {
                    next = (50 + dist, 50, Dir::East)
                } else if pos.1 < 100 {
                    next = (150 + dist, 0, Dir::East)
                } else if pos.1 < 150 {
                    next = (199, 0 + dist, Dir::North)
                } else {
                    panic!("unreachable")
                }
            }
        }
    }

    next
}

fn _draw(map: &HashMap<(i64, i64), bool>, pos: &(i64, i64, Dir)) {
    print!("{}[2J", 27 as char);
    for i in 0..200 {
        for j in 0..200 {
            if i == pos.0 && j == pos.1 {
                print!("X");
                continue;
            }
            match map.get(&(i, j)) {
                Some(&c) => {
                    if c {
                        print!(".")
                    } else {
                        print!("#")
                    }
                }
                None => print!(" "),
            }
        }
        println!()
    }
}

fn part_two(input: &str) -> i64 {
    let (map, moves, mut pos) = parse(input);

    for mov in moves {
        pos = match mov {
            Instr::Right => (pos.0, pos.1, rotate(pos.2, Instr::Right)),
            Instr::Left => (pos.0, pos.1, rotate(pos.2, Instr::Left)),
            Instr::Move(n) => {
                for _ in 0..n {
                    let next = next_cube_wrapping(&map, &pos);
                    match map.get(&(next.0, next.1)) {
                        Some(true) => {
                            // Open space.
                            pos = (next.0, next.1, next.2);
                        }
                        Some(false) => {
                            // Wall.
                            break;
                        }
                        _ => panic!("cube_next broken"),
                    }
                }
                pos
            }
        };
    }

    1000 * (pos.0 + 1) + 4 * (pos.1 + 1) + score(&pos.2)
}

fn main() {
    println!("part 1 input: {}", part_one(_INPUT));
    println!("part 2 input: {}", part_two(_INPUT));
}
