use std::time::Instant;

use cached::proc_macro::cached;
use rayon::prelude::*;
use regex::Regex;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±5ms
        println!("First part: {}", solve(input));
    });

    time(|| {
        // ±15s
        println!("Bonus: {}", bonus(input));
    });
}

fn solve(input: &str) -> usize {
    let re = Regex::new(r"\.+").unwrap();

    input
        .lines()
        .map(|line| {
            let (records, ns) = line.split_once(" ").unwrap();
            arrangements(
                re.split(records)
                    .filter(|s| s.len() > 0)
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>(),
                ns.split(",")
                    .map(|n| n.parse::<usize>().unwrap())
                    .collect::<Vec<_>>(),
            )
        })
        .sum::<usize>()
}

fn bonus(input: &str) -> usize {
    let re = Regex::new(r"\.+").unwrap();

    input
        .lines()
        .collect::<Vec<_>>()
        .into_par_iter()
        .enumerate()
        .map(|(i, line)| {
            let (records, ns) = line.split_once(" ").unwrap();

            let records = format!(
                "{}?{}?{}?{}?{}",
                records, records, records, records, records,
            );

            let ns = format!("{},{},{},{},{}", ns, ns, ns, ns, ns,);

            let num = arrangements(
                re.split(&records)
                    .filter(|s| s.len() > 0)
                    .map(|s| s.to_string())
                    .collect::<Vec<_>>(),
                ns.split(",")
                    .map(|n| n.parse::<usize>().unwrap())
                    .collect::<Vec<_>>(),
            );

            println!("{i} = {num}");

            num
        })
        .sum::<usize>()
}

#[cached]
fn arrangements(pieces: Vec<String>, ns: Vec<usize>) -> usize {
    if ns.is_empty() {
        if pieces.iter().any(|p| p.contains("#")) {
            return 0;
        } else {
            return 1;
        }
    }

    let (i, &n) = ns.iter().enumerate().max_by(|a, b| a.1.cmp(b.1)).unwrap();

    let (ns_le, ns_ri) = split_around(&ns, i);

    pieces
        .iter()
        .enumerate()
        .filter(|(_, piece)| piece.len() >= n)
        .map(|(j, piece)| {
            let (le, ri) = split_around(&pieces, j);

            placements(piece.to_string(), n)
                .into_iter()
                .map(|(p_le, p_ri)| {
                    let num_le = arrangements([le.to_vec(), p_le].concat(), ns_le.to_vec());

                    let num_ri = arrangements([p_ri, ri.to_vec()].concat(), ns_ri.to_vec());

                    num_le * num_ri
                })
                .sum::<usize>()
        })
        .sum::<usize>()
}

fn split_around<T>(items: &[T], i: usize) -> (&[T], &[T]) {
    (&items[0..i], &items[(i + 1)..])
}

#[cached]
fn placements(piece: String, n: usize) -> Vec<(Vec<String>, Vec<String>)> {
    let s = piece.len() - n + 1;

    (0..s)
        .filter_map(move |i| {
            let left_ok = i <= 0 || &piece[(i - 1)..i] == "?";
            let right_ok = i + n >= piece.len() || &piece[(i + n)..(i + n + 1)] == "?";

            if left_ok && right_ok {
                let left = if i >= 2 {
                    vec![piece[0..(i - 1)].to_string()]
                } else {
                    vec![]
                };

                let right = if i + n + 2 <= piece.len() {
                    vec![piece[(i + n + 1)..].to_string()]
                } else {
                    vec![]
                };

                Some((left, right))
            } else {
                None
            }
        })
        .collect::<Vec<_>>()
}

fn time<F>(mut f: F)
where
    F: FnMut(),
{
    let t0 = Instant::now();
    f();
    println!("  took {:?}", t0.elapsed());
}

#[test]
fn test() {
    assert_eq!(
        solve(
            "
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
    "
            .trim(),
        ),
        21
    );

    assert_eq!(
        bonus(
            "
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
    "
            .trim(),
        ),
        525152
    );
}
