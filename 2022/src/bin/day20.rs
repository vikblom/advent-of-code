const _INPUT: &str = include_str!("../../data/input_20.txt");

const _TEST: &str = "1
2
-3
3
-2
0
4";

struct Number {
    val: i64,
    // idx in unencrypted array.
    orig: usize,
}

fn alt(mut nums: Vec<Number>, iter: usize) -> i64 {
    for _ in 0..iter {
        for orig in 0..nums.len() {
            // Find the current idx of the one to move next.
            let idx = nums.iter().position(|v| orig == v.orig).unwrap();

            // Pop and insert at new position, shifting everything in between in the process.
            // Note that "underflow" happens at 0, not -1.
            // For example -2 and index 2 moves the end of the array, not to index 0.
            let needle = nums.remove(idx);
            let length = nums.len() as i64; // Wrap with the number of numbers not counting needle.
            let mut to = (idx as i64 + needle.val) % length;
            if to <= 0 {
                to = length + to;
            }
            nums.insert(to as usize, needle);
        }
    }

    let zero = nums.iter().position(|vv| vv.val == 0).unwrap();
    let a = nums[(zero + 1000) % nums.len()].val;
    let b = nums[(zero + 2000) % nums.len()].val;
    let c = nums[(zero + 3000) % nums.len()].val;

    (a + b + c) as i64
}

fn part_one(input: &str) -> i64 {
    let nums = input
        .lines()
        .map(|l| l.parse().unwrap())
        .enumerate()
        .map(|(orig, val)| Number { val, orig })
        .collect::<Vec<_>>();

    alt(nums, 1)
}

fn part_two(input: &str) -> i64 {
    let nums = input
        .lines()
        .map(|l| l.parse::<i64>().unwrap())
        .enumerate()
        .map(|(orig, val)| Number {
            val: val * 811589153,
            orig,
        })
        .collect::<Vec<_>>();

    alt(nums, 10)
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[test]
fn test_20_p1() {
    assert_eq!(part_one(_TEST), 3);
    assert_eq!(part_one(_INPUT), 13289);
}

#[test]
fn test_20_p2() {
    assert_eq!(part_two(_TEST), 1623178306);
    assert_eq!(part_two(_INPUT), 2865721299243);
}
