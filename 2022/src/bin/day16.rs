#![feature(test)]

use itertools::Itertools;
use regex::Regex;
use std::{
    collections::{BTreeSet, HashMap, HashSet, VecDeque},
    time::Instant,
};

extern crate test;

const _INPUT: &str = include_str!("../../data/input_16.txt");

const _TEST: &str = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II";

// Pack string key into u32.
fn pack(s: &str) -> u32 {
    let mut iter = s.bytes();
    let a = iter.next().unwrap() as u32;
    let b = iter.next().unwrap() as u32;

    1000 * a + b
}

// Unpack u32 to string key.
fn _unpack(n: u32) -> String {
    let mut s = String::new();
    s.push(char::from_u32(n % 1000).unwrap());
    s.push(char::from_u32(n / 1000).unwrap());
    s
}

#[derive(Debug, Clone, Hash)]
struct Node {
    loc: u32,
    opened: BTreeSet<u32>,
    pressure: i32,
    time: i32,
}

fn parse(input: &str) -> (HashMap<u32, i32>, HashMap<u32, Vec<u32>>) {
    // Note "s?" to match both plural/singular.
    let re =
        Regex::new(r"^Valve (\w+) has flow rate=(\d+); tunnels? leads? to valves? ([A-z, ]+)$")
            .unwrap();

    let mut valve_flow = HashMap::new();
    let mut valve_nbrs = HashMap::new();

    for l in input.lines() {
        //println!("{}", l);
        let capt = re.captures(l).unwrap();
        // x -> right
        // y -> down
        let valve = pack(&capt[1]);
        let flow = capt[2].parse::<i32>().unwrap();
        let nbrs = &capt[3].split(", ").map(|s| pack(s)).collect::<Vec<u32>>();
        //println!("{:?} {} {:?}", valve, flow, nbrs);

        valve_flow.insert(valve.clone(), flow);
        valve_nbrs.insert(valve.clone(), nbrs.clone());
    }
    (valve_flow, valve_nbrs)
}

// Compacts the neighbours into shortest path between each non-zero-flow node
// and AA since that is the start.
fn reachability(
    nbrs: &HashMap<u32, Vec<u32>>,
    flow: &HashMap<u32, i32>,
) -> HashMap<u32, Vec<(u32, i32)>> {
    let mut reach = HashMap::new();

    for (k, _) in nbrs {
        if !(*k == pack(&"AA") || *flow.get(k).unwrap() != 0) {
            continue;
        }

        let mut this = Vec::new();

        // BFS - every step takes 1 minute.
        let mut seen = HashSet::new();
        let mut next = VecDeque::new();

        next.push_front((k, 0));

        while let Some((at, dist)) = next.pop_front() {
            if seen.contains(at) {
                continue;
            }
            seen.insert(at);

            if *flow.get(at).unwrap() != 0 && *at != *k {
                this.push((*at, dist));
            }

            for n in nbrs.get(at).unwrap() {
                next.push_back((n, dist + 1));
            }
        }

        reach.insert(*k, this);
    }
    reach
}

// Compacts the neighbours into shortest path between each non-zero-flow node
// and AA since that is the start.
fn reachability2(
    nbrs: &HashMap<u32, Vec<u32>>,
    flow: &HashMap<u32, i32>,
) -> HashMap<(u32, u32), i32> {
    let mut reach = HashMap::new();

    // Distance from each interesting node...
    for (from, _) in nbrs {
        if *from != pack(&"AA") && *flow.get(from).unwrap() == 0 {
            continue;
        }

        // ... to every other interesting node.
        // BFS - every step takes 1 minute.
        let mut seen = HashSet::new();
        let mut next = VecDeque::new();
        next.push_front((from, 0));
        while let Some((to, dist)) = next.pop_front() {
            if seen.contains(to) {
                continue;
            }
            seen.insert(to);

            if *flow.get(to).unwrap() != 0 && *to != *from {
                reach.insert((*from, *to), dist);
            }

            for n in nbrs.get(to).unwrap() {
                next.push_back((n, dist + 1));
            }
        }
    }
    reach
}

// Exhaustive DFS to find the most pressure released, going through the reach(-able) set.
fn dfs(maxtime: i32, flow: &HashMap<u32, i32>, reach: &HashMap<u32, Vec<(u32, i32)>>) -> i64 {
    let mut next = VecDeque::new();
    next.push_back(Node {
        loc: pack(&"AA"),
        opened: BTreeSet::new(),
        pressure: 0,
        time: 0,
    });

    let mut optimum = 0;
    while let Some(node) = next.pop_back() {
        if node.pressure > optimum {
            optimum = node.pressure;
        }

        reach
            .get(&node.loc)
            .unwrap()
            .iter()
            .filter(|(k, v)| !node.opened.contains(k) && (node.time + *v + 1) <= maxtime)
            .for_each(|(to, cost)| {
                let mut opened = node.opened.clone();
                opened.insert(*to);

                // calc how much this vent would contribute to the total.
                let time_active = maxtime - (node.time + cost + 1);
                let contrib = time_active * flow.get(to).unwrap();

                next.push_back(Node {
                    loc: *to,
                    opened,                            // Already added to opened.
                    time: node.time + cost + 1,        // Takes 1 min to open.
                    pressure: node.pressure + contrib, // Get pressure of currently opened when moving there.
                });
            });
    }

    optimum as i64
}

struct Solver {
    reach: HashMap<(u32, u32), i32>, // (pos1, pos2) -> minutes of travel.
    flow: HashMap<u32, i32>,         // flow if pos opened.
    maxtime: i32,

    // Memoize: time spent, current loc, remaining valves.
    memory: HashMap<(i32, u32, Vec<u32>), i64>,
}

impl Solver {
    // Exhaustive DFS to find the most pressure released, going through the reach(-able) set.

    fn dfs(&mut self) -> i64 {
        self.recur(
            pack(&"AA"),
            self.reach
                .keys()
                .map(|&v| v.0)
                .unique()
                .filter(|&v| v != pack("AA"))
                .collect(),
            0,
        )
    }

    fn recur(&mut self, loc: u32, remaining: Vec<u32>, time: i32) -> i64 {
        if let Some(&hit) = self.memory.get(&(time, loc, remaining.clone())) {
            return hit;
        }

        let mut max = 0;
        for (i, &to) in remaining.iter().enumerate() {
            let time = time + 1 + self.reach.get(&(loc, to)).unwrap();
            if time > self.maxtime {
                continue;
            }
            // calc how much this vent would contribute to the total.
            let time_active = self.maxtime - time;
            let pressure = (time_active * self.flow.get(&to).unwrap()) as i64;

            let mut remaining = remaining.clone();
            // Needs to be remove to keep the order, since the memoization is sensitive to order.
            remaining.remove(i);

            let n = pressure + self.recur(to, remaining, time);

            max = std::cmp::max(max, n);
        }

        self.memory.insert((time, loc, remaining), max);
        max
    }
}

fn part_one(input: &str) -> i64 {
    let (flow, neighbours) = parse(input);
    let reach = reachability(&neighbours, &flow);

    let mut best = 0;

    let start = Instant::now();
    let n = dfs(30, &flow, &reach);
    if dbg!(n) > best {
        best = n;
    }
    println!("time: {:.2?}", start.elapsed());

    let start = Instant::now();
    let reach = reachability2(&neighbours, &flow);
    let mut solver = Solver {
        reach,
        flow,
        maxtime: 30,
        memory: HashMap::new(),
    };
    let n = solver.dfs();
    if dbg!(n) > best {
        best = n;
    }
    println!("time: {:.2?}", start.elapsed());
    best as i64
}

// Prune old reachable set to only includes the keep nodes.
fn prune(keep: &Vec<u32>, old: &HashMap<u32, Vec<(u32, i32)>>) -> HashMap<u32, Vec<(u32, i32)>> {
    let mut new = HashMap::new();
    for (&k, v) in old.iter() {
        // Always needs "AA" for the first step.
        if !keep.contains(&k) && k != pack("AA") {
            continue;
        }
        let mut r = Vec::new();
        for &(kk, vv) in v.iter() {
            if !keep.contains(&kk) {
                continue;
            }
            r.push((kk, vv))
        }
        new.insert(k, r);
    }
    new
}

fn part_two(input: &str) -> i64 {
    let (flow, neighbours) = parse(input);
    let reach = reachability2(&neighbours, &flow);
    let keys = reach
        .keys()
        .map(|&v| v.0)
        .filter(|v| *v != pack("AA"))
        .unique()
        .collect::<Vec<u32>>();
    let mut solver = Solver {
        reach,
        flow,
        maxtime: 26,
        memory: HashMap::new(),
    };

    let start = Instant::now();
    // FIXME: Iterator hell, itertools refs the u32s.
    let mut best = 0;
    for n in 1..=(keys.len() / 2) {
        for combination in keys.clone().into_iter().combinations(n) {
            let (mut left, mut right): (Vec<_>, Vec<_>) = keys
                .clone()
                .into_iter()
                .partition(|k| combination.contains(k));

            left.sort();
            right.sort();

            let me = solver.recur(pack(&"AA"), left, 0);
            let elephant = solver.recur(pack(&"AA"), right, 0);
            // let me = dfs(26, &flow, &prune(&left, &reach));
            // let elephant = dfs(26, &flow, &prune(&right, &reach));
            if me + elephant > best {
                best = me + elephant;
            }
        }
    }
    println!("time: {:.2?}", start.elapsed());

    best as i64
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    println!("part 1 input: {}", part_one(_INPUT));

    println!("part 2 test: {}", part_two(_TEST));
    println!("part 2 input: {}", part_two(_INPUT));
}

#[cfg(test)]
mod tests {
    use super::*;
    use test::Bencher;

    #[bench]
    fn bench_part_one(b: &mut Bencher) {
        b.iter(|| part_one(_INPUT));
    }
}