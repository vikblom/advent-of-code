use std::time::Instant;

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

#[derive(Debug)]
struct State {
    // Derive PartialOrd that sorts on states we want.
    ore: i32,
    clay: i32,
    obs: i32,
    geo: i32,

    time: i32,

    ore_bots: i32,
    clay_bots: i32,
    obs_bots: i32,
    geo_bots: i32,
}

fn div_up(a: i32, b: i32) -> i32 {
    (a + (b - 1)) / b
}

fn maximize_geodes(machine: &Machine, maxtime: i32) -> i32 {
    let mut queue = Vec::new();

    queue.push(State {
        time: 0,
        ore: 0,
        clay: 0,
        obs: 0,
        geo: 0,
        ore_bots: 1,
        clay_bots: 0,
        obs_bots: 0,
        geo_bots: 0,
    });

    let max_ore_bots = machine.ore_ore.max(machine.clay_ore).max(machine.obs_ore);
    let max_clay_bots = machine.obs_clay;
    let max_obs_bots = machine.geo_obs;

    let mut best = 0;
    while let Some(state) = queue.pop() {
        let guaranteed = state.geo + state.geo_bots * (maxtime - state.time);
        if guaranteed > best {
            best = guaranteed;
        }
        if state.time == maxtime {
            continue;
        }

        // Is it even possible to beat the best?
        let left = maxtime - state.time;
        if state.geo + state.geo_bots * left + (left * (left - 1)) / 2 < best {
            continue;
        }

        // Instead of moving one minute in each step, building when possible,
        // pick one thing to build and go directly to the time at which it would have
        // been built. Requires some fiddling and picking the limiting resource.
        // Plus 1 minute for the actual build.

        // build ore bot.
        let ttb = 1 + div_up((machine.ore_ore - state.ore).max(0), state.ore_bots);
        if state.ore_bots < max_ore_bots && state.time + ttb <= maxtime {
            queue.push(State {
                time: state.time + ttb,
                ore: state.ore + ttb * state.ore_bots - machine.ore_ore,
                ore_bots: state.ore_bots + 1,

                clay: state.clay + ttb * state.clay_bots,
                obs: state.obs + ttb * state.obs_bots,
                geo: state.geo + ttb * state.geo_bots,
                // copy other bots
                ..state
            });
        }

        // build clay bot
        let ttb = 1 + div_up((machine.clay_ore - state.ore).max(0), state.ore_bots);
        if state.clay_bots < max_clay_bots && state.time + ttb <= maxtime {
            queue.push(State {
                time: state.time + ttb,
                ore: state.ore + ttb * state.ore_bots - machine.clay_ore,
                clay_bots: state.clay_bots + 1,

                clay: state.clay + ttb * state.clay_bots,
                obs: state.obs + ttb * state.obs_bots,
                geo: state.geo + ttb * state.geo_bots,
                // copy other bots
                ..state
            });
        }

        if state.clay_bots == 0 {
            // Not even possible to build more advanced bots.
            continue;
        }

        // build obs bot
        let ttb = 1 + std::cmp::max(
            div_up((machine.obs_ore - state.ore).max(0), state.ore_bots),
            div_up((machine.obs_clay - state.clay).max(0), state.clay_bots),
        );
        if state.obs_bots < max_obs_bots && state.time + ttb <= maxtime {
            queue.push(State {
                time: state.time + ttb,
                ore: state.ore + ttb * state.ore_bots - machine.obs_ore,
                clay: state.clay + ttb * state.clay_bots - machine.obs_clay,
                obs_bots: state.obs_bots + 1,

                obs: state.obs + ttb * state.obs_bots,
                geo: state.geo + ttb * state.geo_bots,
                // copy other bots
                ..state
            });
        }

        if state.obs_bots > 0 {
            // build geo bot
            let ttb = 1 + std::cmp::max(
                div_up((machine.geo_ore - state.ore).max(0), state.ore_bots),
                div_up((machine.geo_obs - state.obs).max(0), state.obs_bots),
            );
            if state.time + ttb <= maxtime {
                queue.push(State {
                    time: state.time + ttb,
                    ore: state.ore + ttb * state.ore_bots - machine.geo_ore,
                    obs: state.obs + ttb * state.obs_bots - machine.geo_obs,
                    geo_bots: state.geo_bots + 1,

                    clay: state.clay + ttb * state.clay_bots,
                    geo: state.geo + ttb * state.geo_bots,
                    // copy other bots
                    ..state
                });
            }
        }
    }

    best
}

fn part_one_alt(input: &str) -> i64 {
    let mut sum = 0;
    for m in parse(input) {
        let quality = maximize_geodes(&m, 24);
        sum += m.id * quality;
        println!("machine: {} quality {}", m.id, quality);
    }
    sum as i64
}

fn part_two_alt(input: &str) -> i64 {
    let mut prod = 1;
    for m in parse(input).iter().take(3) {
        let best = maximize_geodes(&m, 32);
        prod *= best as i64;
        println!("machine: {} best: {}", m.id, best);
    }
    prod as i64
}

fn main() {
    let start = Instant::now();
    let p1 = part_one_alt(_INPUT);
    let dur = start.elapsed();
    println!("alt 1 input: {} ({:.2?})", p1, dur);

    let start = Instant::now();
    let p2 = part_two_alt(_INPUT);
    let dur = start.elapsed();
    println!("alt 2 input: {} ({:.2?})", p2, dur);
}

#[test]
fn test_19_p1() {
    assert_eq!(part_one_alt(_INPUT), 1389);
}

#[test]
fn test_19_p2() {
    assert_eq!(part_two_alt(_INPUT), 3003);
}
