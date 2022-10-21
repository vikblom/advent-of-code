use std::{env, fs, process, sync, time};

use anyhow::Result;
use reqwest::{blocking::Client, cookie::Jar, Url};

const TEMPLATE: &str = include_str!("../../data/template.rs");

fn get_input_file(session: &str, day: i32) -> Result<()> {
    let path = format!("./data/input_{:02}.txt", day);
    let mut fh = fs::File::create(path)?;

    let url = format!("https://adventofcode.com/2020/day/{}/input", day).parse::<Url>()?;

    let cookie = format!("session={}", session);
    let jar = Jar::default();
    jar.add_cookie_str(&cookie, &url);

    let client = Client::builder()
        .timeout(time::Duration::from_secs(10))
        .cookie_provider(sync::Arc::new(jar))
        .build()?;
    let mut resp = client.get(url).send()?;
    // TODO: Is it necessary to check if status is good?
    if !resp.status().is_success() {
        return Err(anyhow::anyhow!(
            "response indicating error: {}",
            resp.status()
        ));
    }
    resp.copy_to(&mut fh)?;

    Ok(())
}

fn write_template(day: i32) -> Result<()> {
    let src = TEMPLATE.replace("%%DAY%%", format!("{:02}", day).as_str());
    fs::write(format!("./src/bin/day{:02}.rs", day), src)?;
    Ok(())
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().skip(1).collect();
    if args.len() != 1 {
        println!("usage: mkday <day>");
        process::exit(1);
    }
    let day = args[0].parse::<i32>()?;

    let session = env::var("AOC_SESSION").expect("set AOC_SESSION in env");

    // input
    get_input_file(&session, day)?;

    // src
    write_template(day)?;

    Ok(())
}
