use std::{collections::HashMap, path::PathBuf};

const _INPUT: &str = include_str!("../../data/input_07.txt");

const _TEST: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

fn parse(input: &str) -> HashMap<String, i64> {
    // For each file, increase size of current and all parent directories.
    let mut sizes: HashMap<String, i64> = HashMap::new();
    // Keep track of the current directory.
    let mut path = PathBuf::from("/");

    for l in input.lines().skip(1) {
        let tokens = l.split(" ").collect::<Vec<_>>();
        if tokens[0] == "$" {
            if tokens[1] == "ls" {
                continue;
            }
            // cd
            if tokens[2] == ".." {
                path.pop();
            } else {
                path.push(tokens[2]);
            }
        } else if !l.starts_with("$") && !l.starts_with("dir") {
            if tokens[0] == "dir" {
                continue;
            }
            // file listing
            let size = tokens[0].parse::<i64>().unwrap();
            let mut p = path.clone();
            loop {
                *sizes.entry(p.to_string_lossy().to_string()).or_default() += size;
                if !p.pop() {
                    break;
                };
            }
        }
    }
    sizes
}

fn part_one(input: &str) -> i64 {
    let sizes = parse(input);
    sizes.values().filter(|v| **v < 100_000).sum()
}

fn part_two(input: &str) -> i64 {
    let sizes = parse(input);

    let unused = 70_000_000 - sizes["/"];
    let to_free = 30_000_000 - unused;

    *sizes.values().filter(|v| (**v) > to_free).min().unwrap()
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
