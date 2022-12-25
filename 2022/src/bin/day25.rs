use std::collections::HashMap;

use itertools::Itertools;

const _INPUT: &str = include_str!("../../data/input_25.txt");

const _TEST: &str = "1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122";

fn part_one(input: &str) -> i64 {
    let snafu2dec = HashMap::from([('=', -2), ('-', -1), ('0', 0), ('1', 1), ('2', 2)]);

    let mut dec: i64 = input
        .lines()
        .map(|l| {
            l.chars()
                .rev()
                .enumerate()
                .map(|(i, c)| snafu2dec[&c] * 5i64.pow(i as u32))
                .sum::<i64>()
        })
        .sum();

    let dec2snafu = vec!['0', '1', '2', '=', '-'];

    let mut carry = 0; // 3 and 4 carry a 1, since they are negative.
    let mut snafu = Vec::new();
    while dec > 0 {
        let rem = dec % 5;
        dec = dec / 5;

        let mut this = rem + carry;
        carry = 0;
        if this > 4 {
            this = 0;
            carry = 1;
        }
        if this >= 3 {
            carry = 1;
        }

        snafu.push(dec2snafu[this as usize]);
    }
    println!("{}", snafu.iter().rev().join(""));
    0
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));
}
