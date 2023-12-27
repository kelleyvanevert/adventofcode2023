use std::{collections::VecDeque, time::Instant};

use fxhash::FxHashMap;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±3ms
        println!("First part: {}", solve(input));
    });

    time(|| {
        // ±8ms
        println!("Bonus: {}", bonus(input));
    });
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

    for _ in 0..1000 {
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

fn gcd(a: usize, b: usize) -> usize {
    if b == 0 {
        a
    } else {
        gcd(b, a % b)
    }
}

fn lcm(nums: &[usize]) -> usize {
    if nums.len() == 1 {
        nums[0]
    } else {
        let a = nums[0];
        let b = lcm(&nums[1..]);
        a * b / gcd(a, b)
    }
}

fn bonus(input: &str) -> usize {
    let mut modules: FxHashMap<&str, (char, Vec<&str>)> = FxHashMap::default();
    let mut flop_state = FxHashMap::default();
    let mut conj_sources: FxHashMap<&str, Vec<&str>> = FxHashMap::default();
    let mut conj_state = FxHashMap::default();

    let mut deps: FxHashMap<&str, Vec<&str>> = FxHashMap::default();

    for line in input.lines() {
        let (name, dests) = line.trim().split_once(" -> ").unwrap();
        let ty = name.chars().next().unwrap();
        let name = &name[1..];
        let dests = dests.split(", ").collect::<Vec<_>>();
        modules.insert(name, (ty, dests.clone()));

        // direct dependencies
        for dest in dests {
            // println!("{name} <- {dest}");
            deps.entry(dest).or_default().push(name);
        }
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

    let layer_1 = vec!["rx"];
    println!("layer 1: {:?}", layer_1);

    let layer_2 = layer_1
        .iter()
        .map(|name| {
            assert_eq!(deps[name].len(), 1);
            assert_eq!(modules[deps[name][0]].0, '&');
            deps[name][0]
        })
        .collect::<Vec<_>>();
    println!("layer 2: {:?}", layer_2);

    let layer_3 = layer_2
        .iter()
        .flat_map(|name| deps[name].clone())
        .inspect(|name| assert_eq!(modules[name].0, '&'))
        .collect::<Vec<_>>();
    println!("layer 3: {:?}", layer_3);

    let mut pulses = VecDeque::new();
    let mut num_pulses = [0, 0];

    let mut cycle_lengths = FxHashMap::default();
    let mut seen3 = FxHashMap::default();

    for presses in 1.. {
        pulses.push_front(("button", false, "roadcaster"));

        while let Some((source, pulse, name)) = pulses.pop_back() {
            // println!("{source} --{pulse}--> {name}");
            num_pulses[pulse as usize] += 1;
            if let Some((ty, dests)) = modules.get(name) {
                let pulse_to_send = match *ty {
                    FLIP_FLOP => {
                        if !pulse {
                            Some(
                                *flop_state
                                    .entry(name)
                                    .and_modify(|b: &mut bool| *b = !*b)
                                    .or_insert(true),
                            )
                        } else {
                            None
                        }
                    }
                    CONJUNCTR => {
                        conj_state.insert((source, name), pulse);

                        let all_high = conj_sources[&name]
                            .iter()
                            .all(|&source| *conj_state.get(&(source, name)).unwrap_or(&false));

                        Some(!all_high)
                    }
                    BROADCAST => Some(pulse),
                    _ => unreachable!(),
                };

                if let Some(pulse_to_send) = pulse_to_send {
                    for &dest in dests {
                        {
                            // wishful thinking
                            if pulse_to_send == false && dest == "rx" {
                                return presses;
                            }
                        }

                        {
                            if pulse_to_send == true && dest == "mg" {
                                println!("sending HI pulse to mg from {name}");

                                let seen = *seen3
                                    .entry(name)
                                    .and_modify(|n: &mut usize| *n += 1)
                                    .or_insert(1);

                                if let Some(value) = cycle_lengths.get(&name) {
                                    assert_eq!(presses, seen * value);
                                } else {
                                    cycle_lengths.insert(name, presses);
                                }

                                if layer_3
                                    .iter()
                                    .all(|&name| cycle_lengths.contains_key(&name))
                                {
                                    return lcm(&layer_3
                                        .iter()
                                        .map(|&name| cycle_lengths.get(&name).cloned().unwrap())
                                        .collect::<Vec<_>>());
                                }
                            }
                        }

                        pulses.push_front((name, pulse_to_send, dest));
                    }
                }
            }
        }
    }

    unreachable!()
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
