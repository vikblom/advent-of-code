use std::collections::HashMap;

const _INPUT: &str = include_str!("../../data/input_21.txt");

const _TEST: &str = "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32";

#[derive(Debug, Clone, Hash)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Hash)]
enum Job {
    Num(i64),
    Math(String, Operator, String),
}

fn recurse(at: &String, monkeys: &HashMap<&str, Job>) -> i64 {
    let job = &monkeys[at.as_str()];

    let new = match job {
        Job::Num(n) => *n,
        Job::Math(left, Operator::Add, right) => recurse(&left, monkeys) + recurse(&right, monkeys),
        Job::Math(left, Operator::Sub, right) => recurse(&left, monkeys) - recurse(&right, monkeys),
        Job::Math(left, Operator::Mul, right) => recurse(&left, monkeys) * recurse(&right, monkeys),
        Job::Math(left, Operator::Div, right) => recurse(&left, monkeys) / recurse(&right, monkeys),
    };
    // dbg!(at, new);
    new
}

fn part_one(input: &str) -> i64 {
    let mut monkeys = HashMap::new();

    for l in input.lines() {
        let (name, rest) = l.split_once(": ").unwrap();

        if let Some(number) = rest.parse::<i64>().ok() {
            monkeys.insert(name, Job::Num(number));
        } else {
            let rest = rest.split(" ").collect::<Vec<_>>();
            let op = match rest[1] {
                "+" => Operator::Add,
                "-" => Operator::Sub,
                "*" => Operator::Mul,
                "/" => Operator::Div,
                _ => panic!("bad input"),
            };
            monkeys.insert(
                name,
                Job::Math(rest[0].to_string(), op, rest[2].to_string()),
            );
        }
    }

    recurse(&"root".to_string(), &monkeys)
}

fn has_humn(at: &str, monkeys: &HashMap<&str, Job>) -> bool {
    if at == "humn" {
        return true;
    }
    if let Some(Job::Math(left, _, right)) = monkeys.get(at) {
        if left == "humn" || right == "humn" {
            true
            //
        } else {
            has_humn(left, monkeys) || has_humn(right, monkeys)
        }
    } else {
        false
    }
}

fn drill_down(at: &str, target: i64, monkeys: &HashMap<&str, Job>) -> i64 {
    if at == "humn" {
        return target;
    }

    if let Some(Job::Math(left, op, right)) = monkeys.get(at) {
        // Calc the knowable branch so the target can be adjusted.
        let other_term: i64;
        let next: &str;
        if has_humn(left, monkeys) {
            other_term = recurse(right, monkeys);
            next = left;
        } else {
            other_term = recurse(left, monkeys);
            next = right;
        };
        // dbg!(at, target, other_term);

        match op {
            // Commutative
            Operator::Add => drill_down(next, target - other_term, monkeys),
            Operator::Mul => drill_down(next, target / other_term, monkeys),
            // Non-commutative
            Operator::Sub => {
                if next == left {
                    // next - other == target
                    drill_down(next, target + other_term, monkeys)
                } else {
                    // other - next == target
                    drill_down(next, other_term - target, monkeys)
                }
            }
            Operator::Div => {
                if next == left {
                    // next / other == target
                    drill_down(next, target * other_term, monkeys)
                } else {
                    // other / next == target
                    drill_down(next, other_term / target, monkeys)
                }
            }
        }
    } else {
        panic!("unexpected branch, should only go towards humn")
    }
}

fn part_two(input: &str) -> i64 {
    let mut monkeys = HashMap::new();

    for l in input.lines() {
        let (name, rest) = l.split_once(": ").unwrap();

        if let Some(number) = rest.parse::<i64>().ok() {
            monkeys.insert(name, Job::Num(number));
        } else {
            let rest = rest.split(" ").collect::<Vec<_>>();
            let mut op = match rest[1] {
                "+" => Operator::Add,
                "-" => Operator::Sub,
                "*" => Operator::Mul,
                "/" => Operator::Div,
                _ => panic!("bad input"),
            };

            // Equal => a-b == 0, which is simpler to start from.
            if name == "root" {
                op = Operator::Sub;
            }

            monkeys.insert(
                name,
                Job::Math(rest[0].to_string(), op, rest[2].to_string()),
            );
        }
    }

    // Patched to "root: a-b", should be zero if equal.
    drill_down("root", 0, &monkeys)
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[test]
fn test_21_p1() {
    assert_eq!(part_one(_TEST), 152);
    assert_eq!(part_one(_INPUT), 145167969204648);
}

#[test]
fn test_21_p2() {
    assert_eq!(part_two(_TEST), 301);
    assert_eq!(part_two(_INPUT), 3330805295850);
}
