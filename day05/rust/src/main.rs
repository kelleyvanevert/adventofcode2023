use std::time::Instant;

fn main() {
    let input = "
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
";

    let input = include_str!("../../input.txt");

    time(|| {
        // ±100µs
        solve(input);
    });

    time(|| {
        // ±50µs
        bonus(input);
    });
}

fn solve(input: &str) {
    let mut groups = input.trim().split("\n\n");
    let seeds = groups
        .next()
        .unwrap()
        .replace("seeds: ", "")
        .split(" ")
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    struct Rule {
        source: usize,
        dest: usize,
        num: usize,
    }

    let mappers = groups
        .map(|group| {
            let rules = group
                .lines()
                .into_iter()
                .skip(1)
                .map(|line| {
                    let mut nums = line.split(" ").map(|s| s.parse::<usize>().unwrap());
                    Rule {
                        dest: nums.next().unwrap(),
                        source: nums.next().unwrap(),
                        num: nums.next().unwrap(),
                    }
                })
                .collect::<Vec<_>>();

            move |n: usize| {
                for rule in &rules {
                    if n >= rule.source && n < rule.source + rule.num {
                        return rule.dest + (n - rule.source);
                    }
                }

                return n;
            }
        })
        .collect::<Vec<_>>();

    let solution = seeds
        .into_iter()
        .map(|mut seed| {
            for mapper in &mappers {
                seed = mapper(seed);
            }
            seed
        })
        .min()
        .unwrap();

    println!("Solution: {solution}");
}

fn bonus(input: &str) {
    let mut groups = input.trim().split("\n\n");
    let seeds = groups
        .next()
        .unwrap()
        .replace("seeds: ", "")
        .split(" ")
        .map(|s| s.parse::<usize>().unwrap())
        .collect::<Vec<_>>();

    struct Rule {
        source: usize,
        dest: usize,
        num: usize,
    }

    let mappers = groups
        .map(|group| {
            let mut rules = group
                .lines()
                .into_iter()
                .skip(1)
                .map(|line| {
                    let mut nums = line.split(" ").map(|s| s.parse::<usize>().unwrap());
                    Rule {
                        dest: nums.next().unwrap(),
                        source: nums.next().unwrap(),
                        num: nums.next().unwrap(),
                    }
                })
                .collect::<Vec<_>>();

            rules.sort_by_key(|t| t.source);

            move |n: usize| {
                for rule in &rules {
                    if n < rule.source {
                        return (n, rule.source - n);
                    }

                    if n >= rule.source && n < rule.source + rule.num {
                        return (rule.dest + (n - rule.source), rule.source + rule.num - n);
                    }
                }

                return (n, 999999999);
            }
        })
        .collect::<Vec<_>>();

    let mut loc = usize::MAX;

    for chunk in seeds.chunks_exact(2) {
        let mut seed = chunk[0];
        let num = chunk[1];
        let end = seed + num;

        while seed < end {
            let mut n = seed;
            let mut skip = usize::MAX;
            for mapper in &mappers {
                let t = mapper(n);
                n = t.0;
                skip = t.1.min(skip);
            }

            loc = loc.min(n);
            seed += skip
        }
    }

    println!("Bonus: {loc}");
}

fn time<F>(mut f: F)
where
    F: FnMut(),
{
    let t0 = Instant::now();
    f();
    println!("  took {:?}", t0.elapsed());
}
