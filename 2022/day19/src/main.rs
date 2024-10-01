use std::io::{self, Read};

use im::Vector;
use regex::Regex;

#[derive(Debug, Clone, Copy)]
enum Resource {
    Ore = 0,
    Clay = 1,
    Obsidian = 2,
    Geode = 3,
}

impl Resource {
    pub const ALL: [Resource; 4] = [
        Resource::Ore,
        Resource::Clay,
        Resource::Obsidian,
        Resource::Geode,
    ];
}

type Amount = u16;
type Time = u16;

type Resources = [Amount; 4];

macro_rules! resource_map {
    ($func:expr, $($args:expr),+ $(,)?) => {{
        [
            $func($($args[0]),+),
            $func($($args[1]),+),
            $func($($args[2]),+),
            $func($($args[3]),+),
        ]
    }};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct State {
    resources: Resources,
    robots: Resources,
}

#[derive(Debug)]
struct Blueprints {
    id: u8,
    blueprints: [Resources; 4],
    max_requirements: Resources,
}

impl Blueprints {
    pub fn new(id: u8, blueprints: [Resources; 4]) -> Blueprints {
        let mut max_requirements = resource_map!(
            |r_type| blueprints.iter().map(|b| b[r_type as usize]).max().unwrap(),
            Resource::ALL,
        );
        // Can't have too many geodes
        max_requirements[Resource::Geode as usize] = Amount::MAX;
        Blueprints {
            id,
            blueprints,
            max_requirements,
        }
    }
}

#[derive(Debug, Clone, Copy)]
struct Action {
    wait_time: Time,
    robot_type: Resource,
}

#[derive(Debug, Clone)]
struct HistoryState {
    action: Action,
    state: State,
}

type History = Vector<HistoryState>;

struct Score {
    score: Amount,
    // history: History,
}

fn checked_divide(a: Amount, b: Amount) -> Option<f32> {
    if b == 0 {
        if a == 0 {
            Some(0.0)
        } else {
            None
        }
    } else {
        Some((a as f32) / (b as f32))
    }
}

fn time_until_able(blueprints: &Blueprints, state: &State, robot_type: Resource) -> Option<Time> {
    let b: Resources = blueprints.blueprints[robot_type as usize];
    let remaining = resource_map!(Amount::saturating_sub, b, state.resources);
    let minutes_remaining = resource_map!(checked_divide, remaining, state.robots);
    let mut max_minutes = 0;
    for maybe_minutes in minutes_remaining.iter() {
        if let Some(m) = maybe_minutes {
            max_minutes = max_minutes.max(m.ceil() as Time);
        } else {
            // Fail if any are missing
            return None;
        }
    }

    Some(max_minutes)
}

fn get_possible_actions<'a>(
    blueprints: &'a Blueprints,
    state: &'a State,
) -> impl Iterator<Item = Action> + 'a {
    Resource::ALL.iter().filter_map(|&r| {
        // No need to build more robots than the maximum cost per type
        if state.robots[r as usize] >= blueprints.max_requirements[r as usize] {
            return None;
        }
        let wait_time = time_until_able(blueprints, state, r);
        wait_time.map(|w| Action {
            wait_time: w,
            robot_type: r,
        })
    })
}

fn calc_blueprint_score(
    blueprints: &Blueprints,
    state: &State,
    time_remaining: Time,
    history: &History,
) -> Score {
    // Score if we did nothing but wait
    let mut best_score = Score {
        score: state.resources[Resource::Geode as usize]
            + state.robots[Resource::Geode as usize] * time_remaining,
        // history: history.clone(),
    };

    if time_remaining <= 1 {
        // No time to do anything
        return best_score;
    }

    for action in get_possible_actions(blueprints, state) {
        if action.wait_time + 1 >= time_remaining {
            continue;
        }

        let new_resources = resource_map!(
            |existing, generated, used| existing + generated * (action.wait_time + 1) - used,
            state.resources,
            state.robots,
            blueprints.blueprints[action.robot_type as usize],
        );
        let mut new_robots = state.robots.clone();
        new_robots[action.robot_type as usize] += 1;

        let new_state = State {
            resources: new_resources,
            robots: new_robots,
        };

        // let mut new_history = history.clone();
        // new_history.push_back(HistoryState {
        //     action,
        //     state: new_state,
        // });
        let branch_score = calc_blueprint_score(
            blueprints,
            &new_state,
            time_remaining - action.wait_time - 1,
            history, //&new_history,
        );

        if branch_score.score > best_score.score {
            best_score = branch_score;
        }
    }

    best_score
}

fn calc_blueprint_quality(blueprints: &Blueprints, maximum_time: Time) -> Amount {
    let init_resources = State {
        resources: [0; 4],
        robots: [1, 0, 0, 0],
    };

    let Score { score, /*history*/ } = calc_blueprint_score(
        blueprints,
        &init_resources,
        maximum_time,
        &Vector::default(),
    );

    // let mut elapsed = 0;
    // for step in history {
    //     println!("== Minute {} ==", elapsed + 1);
    //     if step.action.wait_time > 0 {
    //         println!("Wait for {} minutes", step.action.wait_time);
    //     }
    //     elapsed += step.action.wait_time + 1;
    //     println!("Build {:?} robot", step.action.robot_type);
    //     println!("Resources: {:?}", step.state.resources);
    //     println!("Robots: {:?}", step.state.robots);
    //     println!();
    // }
    let qual = score; // * blueprints.id as Amount;
    println!("Quality for {}: {}", blueprints.id, qual);

    return qual;
}

fn read_input() -> io::Result<Vec<Blueprints>> {
    let pattern = Regex::new(
        "Blueprint (\\d+):\\s*\
            Each ore robot costs (\\d+) ore.\\s*\
            Each clay robot costs (\\d+) ore.\\s*\
            Each obsidian robot costs (\\d+) ore and (\\d+) clay.\\s*\
            Each geode robot costs (\\d+) ore and (\\d+) obsidian.\\s*\
        ",
    )
    .unwrap();
    let mut input = String::default();
    io::stdin().read_to_string(&mut input)?;

    let result = pattern
        .captures_iter(&input)
        .map(|m| {
            let id = m[1].parse::<u8>().unwrap();
            let ore_ore = m[2].parse::<Amount>().unwrap();
            let cly_ore = m[3].parse::<Amount>().unwrap();
            let obs_ore = m[4].parse::<Amount>().unwrap();
            let obs_cly = m[5].parse::<Amount>().unwrap();
            let geo_ore = m[6].parse::<Amount>().unwrap();
            let geo_obs = m[7].parse::<Amount>().unwrap();
            Blueprints::new(
                id,
                [
                    [ore_ore, 0, 0, 0],
                    [cly_ore, 0, 0, 0],
                    [obs_ore, obs_cly, 0, 0],
                    [geo_ore, 0, geo_obs, 0],
                ],
            )
        })
        .collect();

    Ok(result)
}

fn main() {
    let blueprint_sets = read_input().expect("Invalid input");

    let mut total = 1;
    for b in blueprint_sets.iter() {
        let qual = calc_blueprint_quality(b, 32);
        total *= qual;
    }

    println!("Total quality: {}", total);
}
