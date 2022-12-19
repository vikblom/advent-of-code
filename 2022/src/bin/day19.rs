use std::{collections::BTreeMap, time::Instant};

use regex::Regex;

const _INPUT: &str = include_str!("../../data/input_19.txt");

const _TEST: &str = "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.";

#[derive(Debug, Clone)]
struct Machine {
    id: i32,
    ore_ore: i32,
    clay_ore: i32,
    obs_ore: i32,
    obs_clay: i32,
    geo_ore: i32,
    geo_obs: i32,
}

fn parse(input: &str) -> Vec<Machine> {
    let re = Regex::new(r"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.").unwrap();

    let mut machines = Vec::new();
    for l in input.lines() {
        let caps = re.captures(l).unwrap();
        let m = Machine {
            id: caps[1].parse().unwrap(),
            ore_ore: caps[2].parse().unwrap(),
            clay_ore: caps[3].parse().unwrap(),
            obs_ore: caps[4].parse().unwrap(),
            obs_clay: caps[5].parse().unwrap(),
            geo_ore: caps[6].parse().unwrap(),
            geo_obs: caps[7].parse().unwrap(),
        };
        machines.push(m);
    }
    machines
}

#[derive(Debug, Default, Clone, Hash, Ord, Eq, PartialEq, PartialOrd)]
struct State {
    // Derive PartialOrd that sorts on states we want.
    geo: i32,
    obs: i32,
    clay: i32,
    ore: i32,

    time: i32,

    ore_bots: i32,
    clay_bots: i32,
    obs_bots: i32,
    geo_bots: i32,
}

fn alt(machine: &Machine, maxtime: i32) -> i32 {
    let mut queue = BTreeMap::new();

    queue.insert(
        State {
            time: 0,
            ore: 0,
            clay: 0,
            obs: 0,
            geo: 0,
            ore_bots: 1,
            clay_bots: 0,
            obs_bots: 0,
            geo_bots: 0,
        },
        (),
    );

    let max_ore_bots = machine.ore_ore.max(machine.clay_ore).max(machine.obs_ore);
    let max_clay_bots = machine.obs_clay;
    let max_obs_bots = machine.geo_obs;

    let mut best = 0;
    while let Some((state, _)) = queue.pop_first() {
        if state.geo > best {
            best = state.geo;
        }
        if state.time == maxtime {
            continue;
        }

        // If we have zero geo bots, and it would cost more than we can get, just skip.
        let left = maxtime - state.time;
        if state.geo_bots == 0 && (left * (left - 1)) / 2 + state.obs < machine.geo_obs {
            continue;
        }

        // This is what we get next timestep as a base.
        let next = State {
            time: state.time + 1,
            ore: state.ore + state.ore_bots,
            clay: state.clay + state.clay_bots,
            obs: state.obs + state.obs_bots,
            geo: state.geo + state.geo_bots,
            ..state
        };
        // We could use resources to build a bot.
        // Greedily build a geo bot if possible, and if so then only that bot.
        if state.obs >= machine.geo_obs && state.ore >= machine.geo_ore {
            queue.insert(
                State {
                    ore: state.ore + state.ore_bots - machine.geo_ore,
                    obs: state.obs + state.obs_bots - machine.geo_obs,
                    geo_bots: state.geo_bots + 1,
                    ..next
                },
                (),
            );
        } else {
            // Obsidian bot
            if state.clay >= machine.obs_clay
                && state.ore >= machine.obs_ore
                && state.obs_bots < max_obs_bots
            {
                queue.insert(
                    State {
                        ore: state.ore + state.ore_bots - machine.obs_ore,
                        clay: state.clay + state.clay_bots - machine.obs_clay,
                        obs_bots: state.obs_bots + 1,
                        ..next
                    },
                    (),
                );
            }
            // Clay bot
            if state.ore >= machine.clay_ore && state.clay_bots < max_clay_bots {
                queue.insert(
                    State {
                        ore: state.ore + state.ore_bots - machine.clay_ore,
                        clay_bots: state.clay_bots + 1,
                        ..next
                    },
                    (),
                );
            }
            // Ore bot
            if state.ore >= machine.ore_ore && state.ore_bots < max_ore_bots {
                queue.insert(
                    State {
                        ore: state.ore + state.ore_bots - machine.ore_ore,
                        ore_bots: state.ore_bots + 1,
                        ..next
                    },
                    (),
                );
            }

            // Or we just have to wait.
            queue.insert(next, ());
        }
    }

    best
}

fn part_one_alt(input: &str) -> i64 {
    let mut sum = 0;
    for m in parse(input) {
        let quality = alt(&m, 24);
        sum += m.id * quality;
        println!("machine: {} quality {}", m.id, quality);
    }
    sum as i64
}

fn part_two_alt(input: &str) -> i64 {
    let mut prod = 1;
    for m in parse(input).iter().take(3) {
        let best = alt(&m, 32);
        prod *= best as i64;
        println!("machine: {} best: {}", m.id, best);
    }

    prod as i64
}

fn main() {
    // println!("part 1 test: {}", part_one(_TEST));
    // let start = Instant::now();
    // let p1 = part_one(_INPUT);
    // let dur = start.elapsed();
    // println!("part 1 input: {} ({:.2?})", p1, dur);

    let start = Instant::now();
    let p1 = part_one_alt(_INPUT);
    let dur = start.elapsed();
    println!("alt 1 input: {} ({:.2?})", p1, dur);

    // println!("part 2 test: {}", part_two(_TEST));

    // let start = Instant::now();
    // let p2 = part_two(_INPUT);
    // let dur = start.elapsed();
    // println!("part 2 input: {} ({:.2?})", p2, dur);
    let start = Instant::now();
    let p2 = part_two_alt(_INPUT);
    let dur = start.elapsed();
    println!("alt 2 input: {} ({:.2?})", p2, dur);
}
