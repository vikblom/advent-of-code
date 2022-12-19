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

    // Memoize: time
    //memory: HashMap<State, i32>,
    maxtime: i32,
    max_ore_bots: i32,
}

fn parse(input: &str) -> Vec<Machine> {
    let re = Regex::new(r"Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.").unwrap();

    let mut machines = Vec::new();
    for l in input.lines() {
        let caps = re.captures(l).unwrap();
        let mut m = Machine {
            id: caps[1].parse().unwrap(),
            ore_ore: caps[2].parse().unwrap(),
            clay_ore: caps[3].parse().unwrap(),
            obs_ore: caps[4].parse().unwrap(),
            obs_clay: caps[5].parse().unwrap(),
            geo_ore: caps[6].parse().unwrap(),
            geo_obs: caps[7].parse().unwrap(),

            maxtime: 0,
            max_ore_bots: 0,
        };
        m.max_ore_bots = m.ore_ore.max(m.clay_ore).max(m.obs_clay);
        machines.push(m);
    }
    machines
}

pub fn div_up(a: i32, b: i32) -> i32 {
    (a + (b - 1)) / b
}

impl Machine {
    fn recur(&mut self, state: State) -> i32 {
        // Just leave it as is.
        let mut best = state.geo + (self.maxtime - state.time) * state.geo_bots; // max geo

        let left = self.maxtime - state.time;
        if state.geo_bots == 0 && (left * (left - 1)) / 2 + state.obs < self.geo_obs {
            return 0;
        }
        if state.time >= self.maxtime {
            return 0;
        }

        // build ore bot
        let ttb = 1 + div_up((self.ore_ore - state.ore).max(0), state.ore_bots);
        if state.ore_bots < self.max_ore_bots && state.time + ttb <= self.maxtime {
            best = best.max(self.recur(State {
                time: state.time + ttb,
                ore: state.ore + ttb * state.ore_bots - self.ore_ore,
                ore_bots: state.ore_bots + 1,

                clay: state.clay + ttb * state.clay_bots,
                obs: state.obs + ttb * state.obs_bots,
                geo: state.geo + ttb * state.geo_bots,
                // copy other bots
                ..state
            }));
        }

        // build clay bot
        let ttb = 1 + div_up((self.clay_ore - state.ore).max(0), state.ore_bots);
        if state.time + ttb <= self.maxtime {
            best = best.max(self.recur(State {
                time: state.time + ttb,
                ore: state.ore + ttb * state.ore_bots - self.clay_ore,
                clay_bots: state.clay_bots + 1,

                clay: state.clay + ttb * state.clay_bots,
                obs: state.obs + ttb * state.obs_bots,
                geo: state.geo + ttb * state.geo_bots,
                // copy other bots
                ..state
            }));
        }

        if state.clay_bots > 0 {
            // build obs bot
            let ttb = 1 + std::cmp::max(
                div_up((self.obs_ore - state.ore).max(0), state.ore_bots),
                div_up((self.obs_clay - state.clay).max(0), state.clay_bots),
            );
            if state.time + ttb <= self.maxtime {
                best = best.max(self.recur(State {
                    time: state.time + ttb,
                    ore: state.ore + ttb * state.ore_bots - self.obs_ore,
                    clay: state.clay + ttb * state.clay_bots - self.obs_clay,
                    obs_bots: state.obs_bots + 1,

                    obs: state.obs + ttb * state.obs_bots,
                    geo: state.geo + ttb * state.geo_bots,
                    // copy other bots
                    ..state
                }));
            }

            if state.obs_bots > 0 {
                // build geo bot
                let ttb = 1 + std::cmp::max(
                    div_up((self.geo_ore - state.ore).max(0), state.ore_bots),
                    div_up((self.geo_obs - state.obs).max(0), state.obs_bots),
                );
                if state.time + ttb <= self.maxtime {
                    best = best.max(self.recur(State {
                        time: state.time + ttb,
                        ore: state.ore + ttb * state.ore_bots - self.geo_ore,
                        obs: state.obs + ttb * state.obs_bots - self.geo_obs,
                        geo_bots: state.geo_bots + 1,

                        clay: state.clay + ttb * state.clay_bots,
                        geo: state.geo + ttb * state.geo_bots,
                        // copy other bots
                        ..state
                    }));
                }
            }
        }

        best
    }
}

#[derive(Debug, Default, Clone, Eq, PartialEq, Hash)]
struct State {
    time: i32,

    ore: i32,
    clay: i32,
    obs: i32,
    geo: i32,

    ore_bots: i32,
    clay_bots: i32,
    obs_bots: i32,
    geo_bots: i32,
}

fn part_one(input: &str) -> i64 {
    let ms = parse(input);

    // quality = id * max geodes open after 24 minutes.

    let mut sum = 0;

    for mut m in ms {
        m.maxtime = 24;

        let quality = m.recur(State {
            time: 0,
            ore_bots: 1,
            ..Default::default()
        });

        sum += m.id * quality;
        println!("machine: {} quality {}", m.id, quality);
    }

    // sum of all qualities
    sum as i64
}

fn part_two(input: &str) -> i64 {
    let mut ms = parse(input);

    // quality = id * max geodes open after 24 minutes.

    let mut prod = 1;

    for m in ms.iter_mut().take(3) {
        m.maxtime = 32;

        let best = m.recur(State {
            time: 0,
            ore_bots: 1,
            ..Default::default()
        });

        prod *= best as i64;
        println!("machine: {} best: {}", m.id, best);
    }

    prod as i64
}

fn main() {
    println!("part 1 test: {}", part_one(_TEST));
    let start = Instant::now();
    let p1 = part_one(_INPUT);
    let dur = start.elapsed();
    println!("part 1 input: {} ({:.2?})", p1, dur);

    // println!("part 2 test: {}", part_two(_TEST));

    let start = Instant::now();
    let p2 = part_two(_INPUT);
    let dur = start.elapsed();
    println!("part 2 input: {} ({:.2?})", p2, dur);
}
