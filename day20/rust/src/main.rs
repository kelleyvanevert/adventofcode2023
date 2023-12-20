use std::{collections::VecDeque, time::Instant};

use fxhash::FxHashMap;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±3ms
        println!("First part: {}", solve(input));
    });

    // time(|| {
    //     // 9446280843696 is TOO LOW
    //     println!("Bonus (failed attempt): {}", bonus(input));
    // });
}

fn time<F>(f: F)
where
    F: FnOnce(),
{
    let t0 = Instant::now();
    f();
    println!("  took {:?}", t0.elapsed());
}

const FLIP_FLOP: char = '%';
const CONJUNCTR: char = '&';
const BROADCAST: char = 'b';

fn solve(input: &str) -> usize {
    let mut modules = FxHashMap::default();
    let mut flop_state = FxHashMap::default();
    let mut conj_sources: FxHashMap<&str, Vec<&str>> = FxHashMap::default();
    let mut conj_state = FxHashMap::default();

    for line in input.lines() {
        let (name, dests) = line.trim().split_once(" -> ").unwrap();
        let ty = name.chars().next().unwrap();
        modules.insert(&name[1..], (ty, dests.split(", ").collect::<Vec<_>>()));
    }

    for (source, module) in &modules {
        for dest in &module.1 {
            if let Some(m) = modules.get(dest) {
                if m.0 == CONJUNCTR {
                    conj_sources.entry(dest).or_default().push(source);
                }
            }
        }
    }

    let mut pulses = VecDeque::new();
    let mut num_pulses = [0, 0];

    for i in 0..1000 {
        // println!("{i}");
        pulses.push_front(("button", false, "roadcaster"));

        while let Some((source, pulse, name)) = pulses.pop_back() {
            // println!("{source} --{pulse}--> {name}");
            num_pulses[pulse as usize] += 1;
            if let Some((ty, dests)) = modules.get(name) {
                match *ty {
                    FLIP_FLOP => {
                        if !pulse {
                            let new_state = *flop_state
                                .entry(name)
                                .and_modify(|b: &mut bool| *b = !*b)
                                .or_insert(true);

                            for &dest in dests {
                                pulses.push_front((name, new_state, dest));
                            }
                        }
                    }
                    CONJUNCTR => {
                        conj_state.insert((source, name), pulse);

                        let all_high = conj_sources[&name]
                            .iter()
                            .all(|&source| *conj_state.get(&(source, name)).unwrap_or(&false));

                        for &dest in dests {
                            pulses.push_front((name, !all_high, dest));
                        }
                    }
                    BROADCAST => {
                        for &dest in dests {
                            pulses.push_front((name, pulse, dest));
                        }
                    }
                    _ => unreachable!(),
                };
            }
        }
    }

    num_pulses[0] * num_pulses[1]
}

#[test]
fn test() {
    let example_input_1 = "broadcaster -> a, b, c
    %a -> b
    %b -> c
    %c -> inv
    &inv -> a";

    let example_input_2 = "broadcaster -> a
    %a -> inv, con
    &inv -> b
    %b -> con
    &con -> output";

    assert_eq!(solve(example_input_1), 32000000);

    assert_eq!(solve(example_input_2), 11687500);
}
