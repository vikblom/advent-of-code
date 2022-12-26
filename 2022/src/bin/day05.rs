use regex::Regex;

const _INPUT: &str = include_str!("../../data/input_05.txt");

const _TEST: &str = "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2";

fn part_one(input: &str) -> String {
    let (state, moves) = input.split_once("\n\n").unwrap();

    let mut iter = state.lines().rev();
    let n = iter.next().unwrap().split_whitespace().count();
    let mut stacks: Vec<Vec<char>> = Vec::new();
    for _ in 0..n {
        stacks.push(Vec::new());
    }

    for line in iter {
        for c in line.chars().skip(1).step_by(4).enumerate() {
            if c.1 != ' ' {
                stacks[c.0].push(c.1);
            }
        }
    }

    let re = Regex::new(r"^move (\d+) from (\d+) to (\d+)$").unwrap();
    for m in moves.lines() {
        if let Some(capt) = re.captures(m) {
            let num = capt[1].parse::<i64>().unwrap();
            // Note: 1-indexed
            let from = capt[2].parse::<usize>().unwrap();
            let to = capt[3].parse::<usize>().unwrap();

            for _ in 0..num {
                if let Some(x) = stacks[from - 1].pop() {
                    stacks[to - 1].push(x);
                } else {
                    return "".to_string();
                }
            }
        }
    }

    stacks
        .iter_mut()
        .map(|v| v.pop().unwrap())
        .collect::<String>()
}

fn part_two(input: &str) -> String {
    let (state, moves) = input.split_once("\n\n").unwrap();

    let mut iter = state.lines().rev();
    let n = iter.next().unwrap().split_whitespace().count();
    let mut stacks: Vec<Vec<char>> = Vec::new();
    for _ in 0..n {
        stacks.push(Vec::new());
    }

    for line in iter {
        for c in line.chars().skip(1).step_by(4).enumerate() {
            if c.1 != ' ' {
                stacks[c.0].push(c.1);
            }
        }
    }

    let re = Regex::new(r"^move (\d+) from (\d+) to (\d+)$").unwrap();
    for m in moves.lines() {
        if let Some(capt) = re.captures(m) {
            let num = capt[1].parse::<usize>().unwrap();
            // Note: 1-indexed
            let from = capt[2].parse::<usize>().unwrap();
            let to = capt[3].parse::<usize>().unwrap();

            let at = stacks[from - 1].len() - num;
            let mut v = stacks[from - 1].split_off(at);
            stacks[to - 1].append(&mut v);
        }
    }

    stacks
        .iter_mut()
        .map(|v| v.pop().unwrap())
        .collect::<String>()
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[test]
fn test_05_p1() {
    assert_eq!(part_one(_TEST), "CMZ");
    assert_eq!(part_one(_INPUT), "SBPQRSCDF");
}

#[test]
fn test_05_p2() {
    assert_eq!(part_two(_TEST), "MCD");
    assert_eq!(part_two(_INPUT), "RGLVRCQSB");
}
