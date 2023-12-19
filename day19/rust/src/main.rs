use std::time::Instant;

use fxhash::FxHashMap;
use regex::Regex;

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

struct Rule<'a> {
    cond: Option<(&'a str, &'a str, usize)>,
    target: &'a str,
}

fn solve(input: &str) -> usize {
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
}
