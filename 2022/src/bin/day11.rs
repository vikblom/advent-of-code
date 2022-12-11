use std::fmt;

const _INPUT: &str = include_str!("../../data/input_11.txt");
const _TEST: &str = include_str!("../../data/test_11.txt");

struct Monkey {
    items: Vec<u128>,
    is_mul: bool,
    arg: Option<u128>,
    test: u128,
    true_to: u128,
    false_to: u128,

    inspected: u128,
}

impl fmt::Debug for Monkey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("Monkey")
            .field("items", &self.items)
            .field("test", &self.test)
            .finish()
    }
}

impl Monkey {
    fn level(&self, lvl: u128) -> u128 {
        let arg = if self.arg.is_some() {
            self.arg.unwrap()
        } else {
            lvl
        };
        if self.is_mul {
            lvl * arg
        } else {
            lvl + arg
        }
    }
}

fn parse(input: &str) -> Vec<Monkey> {
    let mut monkeys: Vec<Monkey> = Vec::new();
    for lines in input.split("\n\n") {
        let mut iter = lines.lines().skip(1);
        let items = iter
            .next()
            .unwrap()
            .strip_prefix("  Starting items: ")
            .unwrap()
            .split(", ")
            .map(|c| c.parse::<u128>().unwrap())
            .collect::<Vec<_>>();

        let (is_mul, arg) = match iter
            .next()
            .unwrap()
            .strip_prefix("  Operation: new = old ")
            .unwrap()
            .split_once(" ")
        {
            Some(("*", "old")) => (true, None),
            Some(("*", n)) => {
                let n = n.parse::<u128>().unwrap();
                (true, Some(n))
            }
            Some(("+", n)) => {
                let n = n.parse::<u128>().unwrap();
                (false, Some(n))
            }
            _ => panic!("bad input"),
        };

        let test = iter
            .next()
            .unwrap()
            .strip_prefix("  Test: divisible by ")
            .unwrap()
            .parse::<u128>()
            .unwrap();
        let true_to = iter
            .next()
            .unwrap()
            .strip_prefix("    If true: throw to monkey ")
            .unwrap()
            .parse::<u128>()
            .unwrap();
        let false_to = iter
            .next()
            .unwrap()
            .strip_prefix("    If false: throw to monkey ")
            .unwrap()
            .parse::<u128>()
            .unwrap();
        monkeys.push(Monkey {
            items,
            is_mul,
            arg,
            test,
            true_to,
            false_to,
            inspected: 0,
        })
    }
    monkeys
}

fn part_one(input: &str) -> u128 {
    let mut monkeys = parse(input);

    for _ in 0..20 {
        for i in 0..monkeys.len() {
            for lvl in &monkeys[i].items.clone() {
                let lvl = monkeys[i].level(*lvl) / 3;

                let target = if lvl % monkeys[i].test == 0 {
                    monkeys[i].true_to
                } else {
                    monkeys[i].false_to
                };

                monkeys[target as usize].items.push(lvl);
            }
            monkeys[i].inspected += monkeys[i].items.len() as u128;
            monkeys[i].items.truncate(0);
        }
    }

    monkeys.sort_by(|a, b| b.inspected.cmp(&a.inspected));
    monkeys[0].inspected * monkeys[1].inspected
}

fn part_two(input: &str) -> u128 {
    let mut monkeys = parse(input);
    let factor = monkeys.iter().map(|m| m.test).product::<u128>();

    for _ in 0..10_000 {
        for i in 0..monkeys.len() {
            for lvl in &monkeys[i].items.clone() {
                let lvl = monkeys[i].level(*lvl) % factor;

                let target = if lvl % monkeys[i].test == 0 {
                    monkeys[i].true_to
                } else {
                    monkeys[i].false_to
                };

                monkeys[target as usize].items.push(lvl);
            }
            monkeys[i].inspected += monkeys[i].items.len() as u128;
            monkeys[i].items.truncate(0);
        }
    }

    monkeys.sort_by(|a, b| b.inspected.cmp(&a.inspected));
    monkeys[0].inspected * monkeys[1].inspected
}

fn main() {
    println!("part 1 test: {:?}", part_one(_TEST));
    println!("part 1 input: {:?}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
