#![allow(dead_code, unused)]

use std::{fmt::Result, fs::set_permissions, io::stdout, process::exit, sync::Arc, time::Duration};

use reqwest::{blocking::Client, cookie::Jar, Url};

const TEMPLATE: &str = include_str!("../../data/template.rs");

const YEAR: i32 = 2020;

fn copy_day_input<W>(session: &str, day: i32, target: &mut W)
where
    W: std::io::Write,
{
    let url = format!("https://adventofcode.com/2020/day/{}/input", day)
        .parse::<Url>()
        .unwrap();
    let cookie = format!("session={}", session);
    let jar = Jar::default();
    jar.add_cookie_str(&cookie, &url);

    let client = Client::builder()
        .timeout(Duration::from_secs(10))
        .cookie_provider(Arc::new(jar))
        .build()
        .unwrap();
    let mut resp = client.get(url).send().unwrap();
    // TODO: Is it necessary to check if status is good?
    resp.copy_to(target).unwrap();
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    if args.len() != 1 {
        println!("usage: mkday <day>");
        exit(1);
    }
    let day = args[0].parse::<i32>().unwrap();

    let session = std::env::var("AOC_SESSION").expect("set AOC_SESSION in env");

    let path = format!("./data/input_{:02}.txt", day);
    let mut fh = std::fs::File::create(path).unwrap(); // TODO: Match on no such directory.

    // input
    copy_day_input(&session, day, &mut fh);

    // src
    std::fs::write(format!("./src/bin/day{:02}.rs", day), TEMPLATE).unwrap();
}
