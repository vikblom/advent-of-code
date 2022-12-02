use regex::Regex;

const _INPUT: &str = include_str!("../../data/input_02.txt");

const _TEST: &str = "A Y
B X
C Z";

// 0, A, X for Rock,
// 1, B, Y for Paper
// 2, C, Z for Scissors

// Score:
// 1 for Rock
// 2 for Paper
// 3 for Scissors
// plus 0 if you lost, 3 if the round was a draw, and 6 if you won

#[derive(PartialEq, Eq, Copy, Clone)]
enum Move {
    Rock,     // 0
    Paper,    // 1
    Scissors, // 2
}

impl Move {
    fn from_str(s: &str) -> Move {
        match s {
            "A" | "X" => Move::Rock,
            "B" | "Y" => Move::Paper,
            "C" | "Z" => Move::Scissors,
            _ => panic!(),
        }
    }

    // from_i64 wrapping if outside [0,3).
    fn from_i64(mut v: i64) -> Move {
        if v < 0 {
            v += 3;
        }
        v = v % 3;
        match v {
            0 => Move::Rock,
            1 => Move::Paper,
            2 => Move::Scissors,
            _ => panic!(),
        }
    }

    fn score(&self) -> usize {
        match self {
            Move::Rock => 1,
            Move::Paper => 2,
            Move::Scissors => 3,
        }
    }
}

fn part_one(input: &str) -> usize {
    let mut score = 0;
    let re = Regex::new(r"^(\w) (\w)$").unwrap();
    input
        .lines()
        .map(|l| re.captures(l).unwrap())
        .for_each(|v| {
            let them = Move::from_str(&v[1]);
            let us = Move::from_str(&v[2]);

            score += us.score();

            // Equal means draw, if we "one up" them it's a win.
            if us == them {
                score += 3;
            } else if (them as usize + 1) % 3 == us as usize {
                score += 6;
            };
        });

    score
}

// X lose
// Y draw
// Z win

fn part_two(input: &str) -> usize {
    let mut score = 0;
    let re = Regex::new(r"^(\w) (\w)$").unwrap();
    input
        .lines()
        .map(|l| re.captures(l).unwrap())
        .for_each(|v| {
            let them = Move::from_str(&v[1]);
            let us = Move::from_i64({
                // one up is win, same is draw, one less lost.
                let diff = match &v[2] {
                    "X" => -1,
                    "Y" => 0,
                    "Z" => 1,
                    _ => panic!("bad input"),
                };
                them as i64 + diff
            });

            score += us.score();

            // Equal means draw, if we "one up" them it's a win.
            if us == them {
                score += 3;
            } else if (them as usize + 1) % 3 == us as usize {
                score += 6;
            };
        });

    score
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
