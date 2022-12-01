const _INPUT: &str = include_str!("../../data/input_01.txt");

const _TEST: &str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

fn part_one(input: &str) -> usize {
    let mut max = 0;
    // for {

    // }
    let mut sofar = 0;
    let mut lines = input.lines().peekable();
    while let Some(l) = lines.next() {
        //println!("{:?} ({:?})", l, max);
        if l.len() == 0 {
            if sofar > max {
                max = sofar;
            }
            sofar = 0;
        } else {
            sofar += l.parse::<usize>().unwrap()
        }
        // println!("state: {:?} ({:?})", l, max);
    }

    // while let i = lines
    //     .take_while(|l| l.len() > 0)
    //     .map(|i| i.parse::<usize>()
    //     .sum::<usize>()
    // {
    //     println!("{}", i)
    // }
    max
}

fn part_two(input: &str) -> usize {
    let mut elfs = Vec::new();

    let mut sofar = 0;
    let mut lines = input.lines().peekable();
    while let Some(l) = lines.next() {
        //println!("{:?} ({:?})", l, max);
        if l.len() == 0 {
            elfs.push(sofar);
            sofar = 0;
        } else {
            sofar += l.parse::<usize>().unwrap()
        }
        //println!("state: {:?} ({:?})", l, max);
    }
    elfs.push(sofar);

    elfs.sort_by(|a, b| b.cmp(a));
    // println!("{:?}", elfs);
    elfs.iter().take(3).sum()
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    // println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
