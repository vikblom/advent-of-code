const _INPUT: &str = include_str!("../../data/input_10.txt");

//const _TEST: &str = include_str!("../../data/test_10.txt");
const _TEST: &str = "noop
addx 3
addx -5";

fn cpu(input: &str) -> Vec<i64> {
    input.lines().fold(vec![1], |mut st, v| {
        st.push(st[st.len() - 1]);
        if let Some(("addx", add)) = v.split_once(" ") {
            st.push(st.last().unwrap() + add.parse::<i64>().unwrap());
        };
        st
    })
}

fn part_one(input: &str) -> i64 {
    // cycles are 1-indexed, signal is 0-indexed.
    let signal = cpu(input);
    // Intial solution:
    // signal
    //     .iter()
    //     .enumerate()
    //     .filter_map(|(i, v)| {
    //         let c = i + 1;
    //         if c == 20 || c == 60 || c == 100 || c == 140 || c == 180 || c == 220 {
    //             // println!("{}: {:?}", c, v);
    //             Some(v * c as i64)
    //         } else {
    //             None
    //         }
    //     })
    //     .sum()
    (20..221)
        .step_by(40)
        .map(|c| c as i64 * signal[c - 1])
        .sum()
}

fn part_two(input: &str) -> String {
    let signal = cpu(input);

    // reg X is the _horizontal_ pos of the sprite, [0, 40) per row.
    // CRT scans row by row from top left.
    // '#' if sprite (###) overlaps, else '.'
    let mut out = String::new();
    let mut col = 0;
    for v in signal.iter() {
        if (col as i64 - *v).abs() <= 1 {
            out.push('#');
        } else {
            out.push('.');
        }

        col += 1;
        if col == 40 {
            col = 0;
            out.push('\n');
        }
    }

    out
}

fn main() {
    // println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    // println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input:\n{}", part_two(_INPUT));
}

#[test]
fn test_10_p1() {
    assert_eq!(part_one(_INPUT), 10_760);
}

#[test]
fn test_10_p2() {
    let ans = "####.###...##..###..#..#.####..##..#..#.
#....#..#.#..#.#..#.#..#.#....#..#.#..#.
###..#..#.#....#..#.####.###..#....####.
#....###..#.##.###..#..#.#....#.##.#..#.
#....#....#..#.#....#..#.#....#..#.#..#.
#....#.....###.#....#..#.#.....###.#..#.
.";
    assert_eq!(part_two(_INPUT), ans);
}
