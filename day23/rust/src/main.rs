use std::time::Instant;

use fxhash::{FxHashMap, FxHashSet};
use rayon::prelude::*;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±300ms
        println!("First part: {}", solve(input));
    });

    time(|| {
        // ±2s with par_iter in discover_bonus
        // ±9s without par_iter in discover_bonus
        println!("Bonus: {}", bonus(input));
    });
}

fn time<F>(mut f: F)
where
    F: FnMut(),
{
    let t0 = Instant::now();
    f();
    println!("  took {:?}", t0.elapsed());
}

type Pos = (i32, i32);
type Grid<C> = Vec<Vec<C>>;

fn get<C: Copy>(grid: &Grid<C>, (x, y): Pos) -> Option<C> {
    (x >= 0 && y >= 0)
        .then(|| grid.get(y as usize).and_then(|row| row.get(x as usize)))
        .flatten()
        .copied()
}

fn added<C: Copy>(prev: &Vec<C>, next: C) -> Vec<C> {
    let mut prev = prev.clone();
    prev.push(next);
    prev
}

fn discover(grid: &Grid<char>, end: Pos, trail: Vec<Pos>, (x, y): Pos) -> Option<usize> {
    if (x, y) == end {
        return Some(0);
    }

    if trail.contains(&(x, y)) {
        return None;
    }

    match get(grid, (x, y)) {
        Some('>') => return discover(grid, end, added(&trail, (x, y)), (x + 1, y)).map(|n| n + 1),
        Some('<') => return discover(grid, end, added(&trail, (x, y)), (x - 1, y)).map(|n| n + 1),
        Some('v') => return discover(grid, end, added(&trail, (x, y)), (x, y + 1)).map(|n| n + 1),
        Some('^') => return discover(grid, end, added(&trail, (x, y)), (x, y - 1)).map(|n| n + 1),
        _ => {}
    }

    [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
        .iter()
        .filter_map(|&n| {
            if trail.last() == Some(&n) {
                None
            } else {
                match get(grid, n) {
                    Some(c) if c != '#' => discover(grid, end, added(&trail, (x, y)), n),
                    _ => None,
                }
            }
        })
        .map(|n| n + 1)
        .max()
}

fn solve(input: &str) -> usize {
    let grid: Grid<char> = input
        .trim()
        .lines()
        .map(str::chars)
        .map(Iterator::collect)
        .collect();

    let h = grid.len() as i32;
    let w = grid[0].len() as i32;

    let start = (1, 0);

    let end = (w - 2, h - 1);

    discover(&grid, end, vec![], start).unwrap()
}

fn discover_bonus(
    adj: &FxHashMap<Pos, Vec<(usize, Pos)>>,
    end: Pos,
    trail: Vec<Pos>,
    (x, y): Pos,
) -> Option<usize> {
    if (x, y) == end {
        return Some(0);
    }

    if trail.contains(&(x, y)) {
        return None;
    }

    adj.get(&(x, y))
        .unwrap()
        .par_iter()
        .filter_map(|&(length, n)| {
            if trail.last() == Some(&n) {
                None
            } else {
                discover_bonus(adj, end, added(&trail, (x, y)), n).map(|k| k + length)
            }
        })
        .max()
}

fn bonus(input: &str) -> usize {
    let grid: Grid<bool> = input
        .trim()
        .lines()
        .map(|line| line.chars().map(|c| c != '#').collect())
        .collect();

    let h = grid.len() as i32;
    let w = grid[0].len() as i32;

    let start = (1, 0);

    let end = (w - 2, h - 1);

    // let mut vertices = vec![start, end];
    let mut edges = FxHashSet::default();

    let mut todo = vec![vec![start, (1, 1)]];

    while let Some(mut edge) = todo.pop() {
        // println!("exploring edge {:?}", edge);
        let mut prev = edge[0];
        let (mut x, mut y) = edge[1];

        let options = loop {
            let options = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
                .into_iter()
                .filter(|&n| n != prev && get(&grid, n) == Some(true))
                .collect::<Vec<_>>();

            if options.len() == 1 {
                let n = options[0];
                edge.push(n);
                prev = (x, y);
                (x, y) = n;
            } else {
                break options;
            }
        };

        edges.insert(edge.clone());
        edge.reverse();
        edges.insert(edge);

        for n in options {
            if edges
                .iter()
                .position(|edge| {
                    (edge[0] == (x, y) && edge[1] == n)
                        || (edge[edge.len() - 1] == (x, y) && edge[edge.len() - 2] == n)
                })
                .is_some()
            {
                // already considered
            } else {
                todo.push(vec![(x, y), n]);
            }
        }
    }

    let edges = edges
        .into_iter()
        .map(|edge| (edge[0].clone(), edge.len(), edge.last().unwrap().clone()))
        .collect::<Vec<_>>();

    // let vertices = edges.iter().map(|edge| edge.0).collect::<FxHashSet<_>>();

    let mut adj: FxHashMap<Pos, Vec<(usize, Pos)>> = FxHashMap::default();
    for (start, length, end) in edges {
        adj.entry(start).or_default().push((length - 1, end));
    }

    discover_bonus(&adj, end, vec![], start).unwrap()
}

#[test]
fn test() {
    let example_input = "
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
    ";

    assert_eq!(solve(example_input), 94);

    assert_eq!(bonus(example_input), 154);
}
