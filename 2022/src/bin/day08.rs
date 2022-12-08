use std::collections::HashMap;

const _INPUT: &str = include_str!("../../data/input_08.txt");

const _TEST: &str = "30373
25512
65332
33549
35390";

// look_for_trees in trees, putting them in the map of visible trees.
fn look_for_trees(
    trees: &Vec<Vec<(usize, usize, i32)>>,
    visible: &mut HashMap<(usize, usize), bool>,
) {
    for los in trees {
        let mut max = -1;
        for t in los.iter() {
            if t.2 > max {
                max = t.2;
                visible.insert((t.0, t.1), true);
            }
        }
    }
}

fn part_one(input: &str) -> i64 {
    let mut visible = HashMap::<(usize, usize), bool>::new();

    let trees: Vec<_> = input
        .lines()
        .enumerate()
        .map(|(i, l)| {
            l.chars()
                .enumerate()
                // Keep location of tree for de-duplicating hits.
                // Could not figure out how to hook on a mutable state that would
                // carry along across clone()s.
                .map(|(j, c)| (i, j, c.to_digit(10).unwrap() as i32))
                .collect::<Vec<_>>()
        })
        .collect();

    let reverse = trees
        .iter()
        .map(|l| l.into_iter().rev().map(|v| *v).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let transpose = (0..trees[0].len())
        .map(|i| {
            trees
                .iter()
                .map(move |inner| inner[i].clone())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let transpose_reverse = transpose
        .iter()
        .map(|l| l.into_iter().rev().map(|v| *v).collect::<Vec<_>>())
        .collect::<Vec<_>>();

    look_for_trees(&trees, &mut visible);
    look_for_trees(&reverse, &mut visible);
    look_for_trees(&transpose, &mut visible);
    look_for_trees(&transpose_reverse, &mut visible);

    visible.len() as i64
}

fn at(trees: &Vec<Vec<i32>>, row: usize, col: usize) -> Option<&i32> {
    if let Some(row) = trees.get(row) {
        row.get(col)
    } else {
        None
    }
}

fn scenic_score(trees: &Vec<Vec<i32>>, start: (isize, isize), step: (isize, isize)) -> i32 {
    let height = trees[start.0 as usize][start.1 as usize];

    let mut count = 0;
    let (mut i, mut j) = start;
    loop {
        i += step.0;
        j += step.1;

        if let Some(other) = at(trees, i as usize, j as usize) {
            // println!("{} vs {} at [{}, {}]", height, other, i, j);
            count += 1;
            if height <= *other {
                break;
            }
        } else {
            break;
        }
    }
    count
}

fn part_two(input: &str) -> i64 {
    let trees: Vec<_> = input
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| c.to_digit(10).unwrap() as i32)
                .collect::<Vec<_>>()
        })
        .collect();

    let directions: Vec<(isize, isize)> = vec![(1, 0), (-1, 0), (0, 1), (0, -1)];

    let mut best = 0;
    for ti in 1..trees.len() - 1 {
        for tj in 1..trees[0].len() - 1 {
            let score = directions
                .iter()
                .map(|d| scenic_score(&trees, (ti as isize, tj as isize), *d))
                .product();
            if score > best {
                best = score;
            }
        }
    }
    best as i64
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}
