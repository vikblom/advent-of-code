use std::collections::{BTreeSet, HashMap, VecDeque};

const _INPUT: &str = include_str!("../../data/old_18.txt");

const _TEST: &str = "#########
#b.A.@.a#
#########";

// Node represents one state in the state space.
// Optimization: Fit in 8 bytes.
#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct Node {
    loc: (u16, u16),
    keys: BTreeSet<u8>,
}

fn parse(input: &str) -> (HashMap<(u16, u16), u8>, (u16, u16)) {
    let mut map = HashMap::new();
    let mut start = (0, 0);

    for (i, l) in input.lines().enumerate() {
        for (j, c) in l.as_bytes().iter().enumerate() {
            if *c == b'@' {
                start = (i as u16, j as u16);
                map.insert((i as u16, j as u16), b'.');
            } else {
                map.insert((i as u16, j as u16), *c);
            }
        }
    }
    (map, start)
}

fn nbrs(loc: (u16, u16)) -> Vec<(u16, u16)> {
    vec![
        (loc.0 - 1, loc.1),
        (loc.0 + 1, loc.1),
        (loc.0, loc.1 - 1),
        (loc.0, loc.1 + 1),
    ]
}

fn part_one(input: &str) -> Option<u32> {
    let (map, start) = parse(input);
    let total = map.iter().filter(|(_, &v)| v.is_ascii_lowercase()).count();
    let start = Node {
        loc: start,
        keys: BTreeSet::new(),
    };

    println!("total: {}", total);

    // Node (pos+keys) -> best dist so far, used to prune states.
    let mut best: HashMap<Node, u32> = HashMap::new();
    let mut next = VecDeque::new();

    next.push_back((start.clone(), 0));
    while let Some((node, dist)) = next.pop_front() {
        // println!("{:?} -- {}", node, dist);
        if let Some(&better) = best.get(&node) {
            if dist >= better {
                continue;
            }
        }
        best.insert(node.clone(), dist);

        if node.keys.len() == total {
            return Some(dist);
        }

        // For each nbr
        next.extend(nbrs(node.loc).iter().filter_map(|&loc| {
            let mut keys = node.keys.clone();
            match map.get(&loc) {
                Some(b'.') => Some((Node { loc, keys }, dist + 1)),
                Some(c) if c.is_ascii_lowercase() => {
                    keys.insert(*c);
                    Some((Node { loc, keys }, dist + 1))
                }
                Some(c)
                    if c.is_ascii_uppercase() && node.keys.contains(&c.to_ascii_lowercase()) =>
                {
                    Some((Node { loc, keys }, dist + 1))
                }
                _ => None,
            }
        }));
    }

    None
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
struct FourNode {
    one: (u16, u16),
    two: (u16, u16),
    three: (u16, u16),
    four: (u16, u16),
    keys: BTreeSet<u8>,
}

fn part_two(input: &str) -> Option<u32> {
    let (mut map, start) = parse(input);

    // path up the map
    // walls
    map.insert(start, b'#');
    map.insert((start.0 + 1, start.1), b'#');
    map.insert((start.0 - 1, start.1), b'#');
    map.insert((start.0, start.1 + 1), b'#');
    map.insert((start.0, start.1 - 1), b'#');
    // robot starting positions
    map.insert((start.0 + 1, start.1 + 1), b'.');
    map.insert((start.0 + 1, start.1 - 1), b'.');
    map.insert((start.0 - 1, start.1 + 1), b'.');
    map.insert((start.0 - 1, start.1 - 1), b'.');

    let total = map.iter().filter(|(_, &v)| v.is_ascii_lowercase()).count();
    let start = FourNode {
        one: (start.0 + 1, start.1 + 1),
        two: (start.0 + 1, start.1 - 1),
        three: (start.0 - 1, start.1 + 1),
        four: (start.0 - 1, start.1 - 1),
        keys: BTreeSet::new(),
    };

    println!("total: {}", total);

    // Node (pos+keys) -> best dist so far, used to prune states.
    let mut best: HashMap<FourNode, u32> = HashMap::new();
    let mut next = VecDeque::new();

    next.push_back((start.clone(), 0));
    while let Some((node, dist)) = next.pop_front() {
        // println!("{:?} -- {}", node, dist);
        if let Some(&better) = best.get(&node) {
            if dist >= better {
                continue;
            }
        }
        best.insert(node.clone(), dist);

        if node.keys.len() == total {
            return Some(dist);
        }

        // For each nbr
        next.extend(nbrs(node.one).iter().filter_map(|&loc| {
            let mut child = node.clone();
            child.one = loc;
            match map.get(&loc) {
                Some(b'.') => Some((child, dist + 1)),
                Some(c) if c.is_ascii_lowercase() => {
                    child.keys.insert(*c);
                    Some((child, dist + 1))
                }
                Some(c)
                    if c.is_ascii_uppercase() && node.keys.contains(&c.to_ascii_lowercase()) =>
                {
                    Some((child, dist + 1))
                }
                _ => None,
            }
        }));
        next.extend(nbrs(node.two).iter().filter_map(|&loc| {
            let mut child = node.clone();
            child.two = loc;
            match map.get(&loc) {
                Some(b'.') => Some((child, dist + 1)),
                Some(c) if c.is_ascii_lowercase() => {
                    child.keys.insert(*c);
                    Some((child, dist + 1))
                }
                Some(c)
                    if c.is_ascii_uppercase() && node.keys.contains(&c.to_ascii_lowercase()) =>
                {
                    Some((child, dist + 1))
                }
                _ => None,
            }
        }));
        next.extend(nbrs(node.three).iter().filter_map(|&loc| {
            let mut child = node.clone();
            child.three = loc;
            match map.get(&loc) {
                Some(b'.') => Some((child, dist + 1)),
                Some(c) if c.is_ascii_lowercase() => {
                    child.keys.insert(*c);
                    Some((child, dist + 1))
                }
                Some(c)
                    if c.is_ascii_uppercase() && node.keys.contains(&c.to_ascii_lowercase()) =>
                {
                    Some((child, dist + 1))
                }
                _ => None,
            }
        }));
        next.extend(nbrs(node.four).iter().filter_map(|&loc| {
            let mut child = node.clone();
            child.four = loc;
            match map.get(&loc) {
                Some(b'.') => Some((child, dist + 1)),
                Some(c) if c.is_ascii_lowercase() => {
                    child.keys.insert(*c);
                    Some((child, dist + 1))
                }
                Some(c)
                    if c.is_ascii_uppercase() && node.keys.contains(&c.to_ascii_lowercase()) =>
                {
                    Some((child, dist + 1))
                }
                _ => None,
            }
        }));
    }

    None
}

const _TEST2: &str = "#######
#a.#Cd#
##...##
##.@.##
##...##
#cB#Ab#
#######";

const _TEST3: &str = "###############
#d.ABC.#.....a#
######...######
######.@.######
######...######
#b.....#.....c#
###############";

const _TEST4: &str = "#############
#DcBa.#.GhKl#
#.###...#I###
#e#d#.@.#j#k#
###C#...###J#
#fEbA.#.FgHi#
#############";

const _TEST5: &str = "#############
#g#f.D#..h#l#
#F###e#E###.#
#dCba...BcIJ#
#####.@.#####
#nK.L...G...#
#M###N#H###.#
#o#m..#i#jk.#
#############";

fn main() {
    println!("part 1 test: {:?}", part_one(_TEST));
    println!("part 1 input: {:?}", part_one(_INPUT));

    println!("part 2 test: {:?}", part_two(_TEST2));
    println!("part 2 test: {:?}", part_two(_TEST3));
    println!("part 2 test: {:?}", part_two(_TEST4));
    println!("part 2 test: {:?}", part_two(_TEST5));
    // TODO: Blows up in complexity.
    // Need to pre-check accessibility graph, with weighted edges and keys noted.
    // println!("part 2 input: {:?}", part_two(_INPUT));
}
