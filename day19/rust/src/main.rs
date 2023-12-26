use std::time::Instant;

use fxhash::FxHashMap;
use regex::Regex;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±3ms
        println!("First part: {}", solve(input));
    });

    time(|| {
        // ±3ms
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

struct Part {
    x: usize,
    m: usize,
    a: usize,
    s: usize,
}

impl Part {
    fn check(&self, attr: &str, op: &str, val: usize) -> bool {
        let my_val = match attr {
            "x" => self.x,
            "m" => self.m,
            "a" => self.a,
            "s" => self.s,
            _ => unreachable!(),
        };

        match op {
            ">" => my_val > val,
            "<" => my_val < val,
            _ => unreachable!(),
        }
    }

    fn val(&self) -> usize {
        self.x + self.m + self.a + self.s
    }
}

type Cond<'a> = (&'a str, &'a str, usize);

#[derive(Debug, Clone)]
struct Rule<'a> {
    cond: Option<Cond<'a>>,
    target: &'a str,
}

fn parse(input: &str) -> (FxHashMap<&str, Vec<Rule>>, Vec<Part>) {
    let rule_re = Regex::new(r"^([xmas])([><])([0-9]+):([a-zA-Z]+)$").unwrap();
    let part_re = Regex::new(r"x=([0-9]+),m=([0-9]+),a=([0-9]+),s=([0-9]+)").unwrap();
    let mut workflows = FxHashMap::default();
    let mut parts = vec![];
    for line in input.lines() {
        if line.starts_with("{") {
            let c = part_re.captures(line).unwrap();
            let x = c.get(1).unwrap().as_str().parse::<usize>().unwrap();
            let m = c.get(2).unwrap().as_str().parse::<usize>().unwrap();
            let a = c.get(3).unwrap().as_str().parse::<usize>().unwrap();
            let s = c.get(4).unwrap().as_str().parse::<usize>().unwrap();
            parts.push(Part { x, m, a, s });
        } else if line.len() > 0 {
            let (name, rules) = line.split_once("{").unwrap();
            workflows.insert(
                name,
                rules
                    .split(",")
                    .map(|rule| {
                        if rule.ends_with("}") {
                            Rule {
                                cond: None,
                                target: &rule[0..rule.len() - 1],
                            }
                        } else {
                            let c = rule_re.captures(rule).unwrap();
                            Rule {
                                cond: Some((
                                    c.get(1).unwrap().as_str(),
                                    c.get(2).unwrap().as_str(),
                                    c.get(3).unwrap().as_str().parse::<usize>().unwrap(),
                                )),
                                target: c.get(4).unwrap().as_str(),
                            }
                        }
                    })
                    .collect::<Vec<_>>(),
            );
        }
    }

    (workflows, parts)
}

fn solve(input: &str) -> usize {
    let (workflows, parts) = parse(input);

    parts
        .iter()
        .map(|part| {
            let mut at = "in";

            while at != "R" && at != "A" {
                at = workflows[at]
                    .iter()
                    .find_map(|rule| match rule.cond {
                        Some((attr, op, val)) => part.check(attr, op, val).then_some(rule.target),
                        None => Some(rule.target),
                    })
                    .expect("matching rule");
            }

            if at == "A" {
                part.val()
            } else {
                0
            }
        })
        .sum()
}

#[derive(Debug, Clone)]
struct Cube {
    x: (usize, usize),
    m: (usize, usize),
    a: (usize, usize),
    s: (usize, usize),
}

impl Cube {
    fn new() -> Cube {
        Cube {
            x: (1, 4000),
            m: (1, 4000),
            a: (1, 4000),
            s: (1, 4000),
        }
    }

    fn apply_cond((min, max): (usize, usize), op: &str, val: usize) -> (usize, usize) {
        match op {
            ">" => (min.max(val + 1), max),
            ">=" => (min.max(val), max),
            "<" => (min, max.min(val - 1)),
            "<=" => (min, max.min(val)),
            _ => unreachable!(),
        }
    }

    fn from_conds(conds: Vec<Cond>) -> Cube {
        let mut cube = Cube::new();

        for (attr, op, val) in conds {
            match attr {
                "x" => cube.x = Cube::apply_cond(cube.x, op, val),
                "m" => cube.m = Cube::apply_cond(cube.m, op, val),
                "a" => cube.a = Cube::apply_cond(cube.a, op, val),
                "s" => cube.s = Cube::apply_cond(cube.s, op, val),
                _ => unreachable!(),
            }
        }

        cube
    }

    fn count(&self) -> u64 {
        (self.x.1 - self.x.0 + 1) as u64
            * (self.m.1 - self.m.0 + 1) as u64
            * (self.a.1 - self.a.0 + 1) as u64
            * (self.s.1 - self.s.0 + 1) as u64
    }
}

fn opposite(op: &str) -> &str {
    match op {
        ">" => "<=",
        "<" => ">=",
        _ => unreachable!(),
    }
}

fn bonus(input: &str) -> u64 {
    let (workflows, _) = parse(input);

    let mut accepted = vec![];
    let mut todo = vec![(vec![], workflows["in"].clone())];

    while let Some((cube, mut rules)) = todo.pop() {
        let first = rules.remove(0);
        match first.cond {
            Some((attr, op, val)) => {
                let mut cube_left = cube.clone();
                cube_left.push((attr, op, val));
                if first.target == "A" {
                    accepted.push(cube_left);
                } else if let Some(workflow) = workflows.get(first.target) {
                    todo.push((cube_left, workflow.clone()));
                }

                let mut cube_right = cube.clone();
                cube_right.push((attr, opposite(op), val));
                todo.push((cube_right, rules));
            }
            None => {
                if first.target == "A" {
                    accepted.push(cube);
                } else if let Some(workflow) = workflows.get(first.target) {
                    rules.extend(workflow.clone().into_iter());
                    todo.push((cube, rules));
                }
            }
        }
    }

    let cubes = accepted
        .into_iter()
        .map(Cube::from_conds)
        .collect::<Vec<_>>();

    // interestingly, I didn't have to remove any overlaps first,
    //  which I was expecting to be necessary :)

    cubes.iter().map(|cube| cube.count()).sum::<u64>()
}

#[test]
fn test() {
    let example_input = "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}";

    assert_eq!(solve(&example_input), 19114);

    assert_eq!(bonus(&example_input), 167409079868000);
}
