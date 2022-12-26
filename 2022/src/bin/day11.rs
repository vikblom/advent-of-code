use std::cell::RefCell;

const _INPUT: &str = include_str!("../../data/input_11.txt");
//const _TEST: &str = include_str!("../../data/test_11.txt");

#[derive(Debug)]
enum Op {
    Add(i64),
    Mul(i64),
    Square,
}

impl Op {
    fn call(&self, lvl: i64) -> i64 {
        match self {
            Op::Add(n) => lvl + n,
            Op::Mul(n) => lvl * n,
            Op::Square => lvl * lvl,
        }
    }
}

#[derive(Debug)]
struct Monkey {
    items: Vec<i64>,
    op: Op,
    test: i64,

    true_to: usize,
    false_to: usize,
    inspected: usize,
}

fn parse(input: &str) -> Option<Vec<RefCell<Monkey>>> {
    let mut monkeys: Vec<RefCell<Monkey>> = Vec::new();
    for lines in input.split("\n\n") {
        let mut iter = lines.lines().skip(1);
        let items = iter
            .next()?
            .strip_prefix("  Starting items: ")?
            .split(", ")
            .map(|c| c.parse::<i64>().unwrap())
            .collect::<Vec<_>>();

        let op = match iter
            .next()?
            .strip_prefix("  Operation: new = old ")?
            .split_once(" ")
        {
            Some(("*", "old")) => Op::Square,
            Some(("*", n)) => {
                let n = n.parse::<i64>().unwrap();
                Op::Mul(n)
            }
            Some(("+", n)) => {
                let n = n.parse::<i64>().unwrap();
                Op::Add(n)
            }
            _ => panic!("bad input"),
        };

        let test = iter
            .next()?
            .strip_prefix("  Test: divisible by ")?
            .parse::<i64>()
            .ok()?;
        let true_to = iter
            .next()?
            .strip_prefix("    If true: throw to monkey ")?
            .parse::<_>()
            .ok()?;
        let false_to = iter
            .next()?
            .strip_prefix("    If false: throw to monkey ")?
            .parse::<_>()
            .ok()?;

        monkeys.push(RefCell::new(Monkey {
            items,
            op,
            test,
            true_to,
            false_to,
            inspected: 0,
        }))
    }
    Some(monkeys)
}

fn part_one(input: &str) -> usize {
    let mut monkeys = parse(input).unwrap();

    for _ in 0..20 {
        for m in &monkeys {
            let mut m = m.borrow_mut();
            m.inspected += m.items.len();
            while let Some(lvl) = m.items.pop() {
                let lvl = m.op.call(lvl) / 3;
                let target = if lvl % m.test == 0 {
                    m.true_to
                } else {
                    m.false_to
                };
                monkeys[target].borrow_mut().items.push(lvl);
            }
        }
    }

    monkeys.sort_by(|a, b| b.borrow().inspected.cmp(&a.borrow().inspected));
    monkeys
        .iter()
        .map(|m| m.borrow().inspected)
        .take(2)
        .product()
}

fn part_two(input: &str) -> usize {
    let mut monkeys = parse(input).unwrap();
    let factor = monkeys.iter().map(|m| m.borrow().test).product::<i64>();

    for _ in 0..10_000 {
        for m in &monkeys {
            let mut m = m.borrow_mut();
            m.inspected += m.items.len();
            while let Some(lvl) = m.items.pop() {
                let lvl = m.op.call(lvl) % factor;
                let target = if lvl % m.test == 0 {
                    m.true_to
                } else {
                    m.false_to
                };
                monkeys[target].borrow_mut().items.push(lvl);
            }
        }
    }

    monkeys.sort_by(|a, b| b.borrow().inspected.cmp(&a.borrow().inspected));
    monkeys
        .iter()
        .map(|m| m.borrow().inspected)
        .take(2)
        .product()
}

fn main() {
    // println!("part 1 test: {:?}", part_one(_TEST));
    println!("part 1 input: {:?}", part_one(_INPUT));

    // println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[test]
fn test_11_p1() {
    assert_eq!(part_one(_INPUT), 58_056);
}

#[test]
fn test_11_p2() {
    assert_eq!(part_two(_INPUT), 15_048_718_170);
}
