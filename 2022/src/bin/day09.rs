use std::collections::HashMap;

const _INPUT: &str = include_str!("../../data/input_09.txt");

const _TEST: &str = "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";

fn _draw(n: i32, head: (i32, i32), tail: (i32, i32)) {
    for i in (0..n).rev() {
        for j in 0..n {
            match (i, j) {
                a if a == head => print!("H"),
                a if a == tail => print!("T"),
                _ => print!("."),
            }
        }
        println!("");
    }
    println!("");
}

fn abs(x: i32) -> i32 {
    if x < 0 {
        return -x;
    }
    x
}

fn part_one(input: &str) -> i64 {
    // (0,0) is bottom left.
    let mut head = (0, 0);
    let mut tail = (0, 0);

    let mut seen: HashMap<(i32, i32), bool> = HashMap::new();

    for l in input.lines() {
        let (a, b) = l.split_once(" ").unwrap();
        let steps = b.parse::<i32>().unwrap();
        let dir = match a {
            "U" => (1, 0),
            "D" => (-1, 0),
            "R" => (0, 1),
            "L" => (0, -1),
            _ => panic!("incorrect input"),
        };

        // println!("{} {}", a, b);
        for _ in 0..steps {
            seen.insert(tail, true);
            let old = head;
            head = (head.0 + dir.0, head.1 + dir.1);
            if abs(head.0 - tail.0) > 1 || abs(head.1 - tail.1) > 1 {
                tail = old;
            }
            // draw(6, head, tail);
        }
    }

    seen.len() as i64
}

fn _draw2(n: i32, rope: &Vec<(i32, i32)>) {
    for i in (-n..n).rev() {
        'outer: for j in -n..n {
            if (i, j) == rope[0] {
                print!("H");
                continue;
            }
            for k in 1..rope.len() {
                if (i, j) == rope[k] {
                    print!("{}", k);
                    continue 'outer;
                }
            }
            print!(".");
        }
        println!("");
    }
    println!("");
}

fn part_two(input: &str) -> i64 {
    // (0,0) is bottom left.
    //let mut head = (0, 0);
    let mut rope = vec![(0, 0); 10];

    let mut seen: HashMap<(i32, i32), bool> = HashMap::new();
    seen.insert(rope[0], true);

    for l in input.lines() {
        let (a, b) = l.split_once(" ").unwrap();
        let steps = b.parse::<i32>().unwrap();
        let dir = match a {
            "U" => (1, 0),
            "D" => (-1, 0),
            "R" => (0, 1),
            "L" => (0, -1),
            _ => panic!("incorrect input"),
        };

        //println!("{} {}", a, b);
        for _ in 0..steps {
            rope[0] = (rope[0].0 + dir.0, rope[0].1 + dir.1);
            for i in 1..rope.len() {
                let ahead = rope[i - 1];
                let at = rope[i];

                if abs(ahead.0 - at.0) > 1 || abs(ahead.1 - at.1) > 1 {
                    let diff = (ahead.0 - at.0, ahead.1 - at.1);
                    let step = (diff.0.signum(), diff.1.signum());
                    rope[i] = (rope[i].0 + step.0, rope[i].1 + step.1);
                }
            }
            seen.insert(rope[rope.len() - 1], true);
        }
        //draw2(20, &rope);
    }

    seen.len() as i64
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
