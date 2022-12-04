const _INPUT: &str = include_str!("../../data/input_04.txt");

const _TEST: &str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
2-5,15-90";

fn part_one(input: &str) -> i64 {
    let mut score = 0;
    for sec in input.lines().map(|l| {
        l.split(&[',', '-'])
            .map(|v| v.parse::<i64>().unwrap())
            .collect::<Vec<_>>()
    }) {
        if sec[0] <= sec[2] && sec[1] >= sec[3] {
            score += 1;
        } else if (sec[2] <= sec[0]) && (sec[3] >= sec[1]) {
            score += 1;
        }
    }
    score
}

fn part_two(input: &str) -> i64 {
    input
        .lines()
        .map(|l| {
            l.split(&[',', '-'])
                .map(|v| v.parse::<i64>().unwrap())
                .collect::<Vec<_>>()
        })
        .filter(|sec| sec[0] <= sec[3] && sec[1] >= sec[2])
        .count()
        .try_into()
        .unwrap()
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
