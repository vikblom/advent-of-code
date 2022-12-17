use std::collections::{HashMap, HashSet};

const _INPUT: &str = include_str!("../../data/input_17.txt");

const _TEST: &str = "
>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

#[derive(Debug)]
enum Wind {
    Left,
    Right,
}

// Anchor in bottom left corner.
struct Shape {
    parts: Vec<(i64, i64)>,
    height: i64,
    width: i64,
}

fn shapes() -> Vec<Shape> {
    let mut shapes = Vec::new();

    // ####
    shapes.push(Shape {
        parts: vec![(0, 0), (0, 1), (0, 2), (0, 3)],
        height: 1,
        width: 4,
    });

    // .#.
    // ###
    // .#.
    shapes.push(Shape {
        parts: vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
        height: 3,
        width: 3,
    });

    // ..#
    // ..#
    // ###
    shapes.push(Shape {
        parts: vec![(0, 0), (0, 1), (0, 2), (1, 2), (2, 2)],
        height: 3,
        width: 3,
    });

    // #
    // #
    // #
    // #
    shapes.push(Shape {
        parts: vec![(0, 0), (1, 0), (2, 0), (3, 0)],
        height: 4,
        width: 1,
    });

    // ##
    // ##
    shapes.push(Shape {
        parts: vec![(0, 0), (1, 0), (0, 1), (1, 1)],
        height: 2,
        width: 2,
    });

    shapes
}

fn _draw(mut placed: HashMap<(i64, i64), u8>, anchor: &(i64, i64), shape: &Shape) {
    for delta in shape.parts.iter() {
        placed.insert((anchor.0 + delta.0, anchor.1 + delta.1), b'@');
    }

    for i in (0..=(anchor.0 + shape.height + 3)).rev() {
        for j in 0..7 {
            match placed.get(&(i, j)) {
                Some(c) => print!("{}", *c as char),
                None => print!("."),
            }
            //
        }
        println!("");
    }
    println!("");
}

fn collides(placed: &HashMap<(i64, i64), u8>, anchor: &(i64, i64), shape: &Shape) -> bool {
    for delta in shape.parts.iter() {
        if placed.contains_key(&(anchor.0 + delta.0, anchor.1 + delta.1)) {
            return true;
        }
    }
    false
}

fn part_one(input: &str) -> i64 {
    let winds = input
        .trim()
        .bytes()
        .map(|b| match b {
            b'<' => Wind::Left,
            b'>' => Wind::Right,
            _ => panic!("invalid input"),
        })
        .collect::<Vec<_>>();
    let mut next_wind = winds.iter().cycle();

    let mut highest = 0;
    let mut placed = HashMap::new();
    let mut number_of_rocks = 0;
    for shape in shapes().iter().cycle() {
        let mut anchor = (highest + 3, 2);

        while let Some(w) = next_wind.next() {
            // Sidewinds
            let maybe = match w {
                Wind::Left => (anchor.0, anchor.1 - 1),
                Wind::Right => (anchor.0, anchor.1 + 1),
            };
            if 0 <= maybe.1 && (maybe.1 + shape.width <= 7) && !collides(&placed, &maybe, shape) {
                anchor = maybe;
            }

            // Downwards
            let maybe = (anchor.0 - 1, anchor.1);
            if 0 <= maybe.0 && !collides(&placed, &maybe, shape) {
                anchor = maybe;
            } else {
                for delta in shape.parts.iter() {
                    placed.insert((anchor.0 + delta.0, anchor.1 + delta.1), b'#');
                }
                if anchor.0 + shape.height > highest {
                    highest = anchor.0 + shape.height;
                }

                break;
            }
        }

        number_of_rocks += 1;
        if number_of_rocks == 2022 {
            break;
        }
    }

    highest as i64
}

fn snapshot(
    placed: &HashMap<(i64, i64), u8>,
    anchor: &(i64, i64),
    shape: &Shape,
    wind_idx: usize,
) -> String {
    let mut rock = HashSet::new();
    for delta in shape.parts.iter() {
        rock.insert((anchor.0 + delta.0, anchor.1 + delta.1));
    }

    let depth = 10;
    let mut s = String::with_capacity(((depth + shape.height) * 7) as usize);

    s.push_str(&wind_idx.to_string());
    s.push('\n');

    for i in ((anchor.0 - depth)..(anchor.0 + shape.height)).rev() {
        for j in 0..7 {
            if rock.contains(&(i, j)) {
                s.push('@');
            } else if placed.contains_key(&(i, j)) {
                s.push('#');
            } else {
                s.push('.');
            }
        }
        s.push('\n');
    }
    s
}

fn part_two(input: &str) -> i64 {
    let winds = input
        .trim()
        .bytes()
        .map(|b| match b {
            b'<' => Wind::Left,
            b'>' => Wind::Right,
            _ => panic!("invalid input"),
        })
        .collect::<Vec<_>>();
    // Enumerate the pos in the original array so repetition can be found.
    let mut next_wind = winds.iter().enumerate().cycle().peekable();

    let mut seen = HashMap::new();
    let mut heights: Vec<i64> = Vec::new();
    let mut placed = HashMap::new();
    for (i, shape) in shapes().iter().cycle().enumerate() {
        let mut anchor = (*heights.last().unwrap_or(&0) + 3, 2);

        // A new falling rock.
        while let Some((_, w)) = next_wind.next() {
            // Sidewinds
            let maybe = match w {
                Wind::Left => (anchor.0, anchor.1 - 1),
                Wind::Right => (anchor.0, anchor.1 + 1),
            };
            if 0 <= maybe.1 && (maybe.1 + shape.width <= 7) && !collides(&placed, &maybe, shape) {
                anchor = maybe;
            }

            // Downwards
            let maybe = (anchor.0 - 1, anchor.1);
            if 0 <= maybe.0 && !collides(&placed, &maybe, shape) {
                anchor = maybe;
            } else {
                break;
            }
        }

        // Rock has stopped moving, lock it in.
        for delta in shape.parts.iter() {
            placed.insert((anchor.0 + delta.0, anchor.1 + delta.1), b'#');
        }
        heights.push(std::cmp::max(
            anchor.0 + shape.height,
            *heights.last().unwrap_or(&0),
        ));

        // Snapshot current state, if we find a repetition, assume that
        // the segment since that earlier snapshot will repeat again and again.
        let snap = snapshot(&placed, &anchor, shape, next_wind.peek().unwrap().0);
        if let Some(&earlier) = seen.get(&snap) {
            let seg_rocks = (i - earlier) as i64;
            let seg_height = heights[i] - heights[earlier];

            // From index to number of rocks to drop in repeated segments.
            let remaining = 1000000000000 - (earlier + 1) as i64;

            // There will be a partial segment at the end.
            // Lookup how high that would go in rem rocks from the segment since earlier.
            let repeated = (remaining / seg_rocks) * seg_height;
            let fin = heights[earlier + (remaining % seg_rocks) as usize] - heights[earlier];

            return heights[earlier] + repeated + fin;
        }
        seen.insert(snap, i);
    }
    unreachable!();
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
