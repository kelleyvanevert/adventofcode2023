#![feature(let_chains)]

use core::panic;
use std::{
    collections::{BinaryHeap, HashSet},
    iter::once,
    time::Instant,
};

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±500ms
        println!("First part: {}", solve(input));
    });

    time(|| {
        // ±6s
        println!("Bonus: {}", bonus(input));
    });
}

fn get_padded_grid(input: &str) -> Grid {
    let w = input.lines().next().unwrap().len();

    once(vec![HI; w + 2])
        .chain(
            input
                .lines()
                .map(|line| {
                    let mut row = line
                        .chars()
                        .map(|c| c as usize - '0' as usize)
                        .collect::<Vec<_>>();
                    row.insert(0, HI);
                    row.push(HI);
                    row
                })
                .chain(once(vec![HI; w + 2])),
        )
        .collect::<Vec<_>>()
}

const HI: usize = 10;

type Dir = usize;
type Pos = (usize, usize);
type Grid = Vec<Vec<usize>>;

const NOOP: Dir = 0;
const UP: Dir = 1;
const RIGHT: Dir = 2;
const DOWN: Dir = 3;
const LEFT: Dir = 4;

fn step((x, y): Pos, dir: Dir) -> Pos {
    match dir {
        UP => (x, y - 1),
        RIGHT => (x + 1, y),
        DOWN => (x, y + 1),
        LEFT => (x - 1, y),
        _ => unreachable!(),
    }
}

fn opposite(dir: Dir) -> Dir {
    match dir {
        UP => DOWN,
        RIGHT => LEFT,
        DOWN => UP,
        LEFT => RIGHT,
        _ => unreachable!(),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Path {
    target: (usize, usize),
    head: (usize, usize),
    prev: [Dir; 3], // [last, before that, and before that]
    cost_so_far: usize,
}

impl Path {
    fn manhattan_to_target(&self) -> usize {
        let (tx, ty) = self.target;
        let (x, y) = self.head;

        (tx - x) + (ty - y)
    }

    fn score(&self) -> usize {
        self.cost_so_far + self.manhattan_to_target()
    }

    fn next(&self, grid: &Grid) -> Vec<Path> {
        let mut next = vec![];

        for dir in [UP, RIGHT, DOWN, LEFT] {
            if opposite(dir) == self.prev[0] {
                // can't reverse
                continue;
            }

            if self.prev.iter().all(|&d| d == dir) {
                // cannot go in this diration any longer
                continue;
            }

            let (x, y) = step(self.head, dir);
            if grid[y][x] >= 10 {
                // stay in bounds
                continue;
            }

            let mut prev = self.prev.clone();
            prev.rotate_right(1);
            prev[0] = dir;

            next.push(Path {
                target: self.target,
                head: (x, y),
                prev,
                cost_so_far: self.cost_so_far + grid[y][x],
            });
        }

        next
    }
}

impl PartialOrd for Path {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&other))
    }
}

impl Ord for Path {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.score().cmp(&other.score()).reverse()
    }
}

// using greedy best-first search
fn solve(input: &str) -> usize {
    let grid = get_padded_grid(input);
    let h = grid.len();
    let w = grid[0].len();

    let mut seen = HashSet::new();

    let mut paths = BinaryHeap::new();
    paths.push(Path {
        target: (w - 2, h - 2),
        head: (1, 1),
        prev: [NOOP; 3],
        cost_so_far: 0,
    });

    while let Some(at) = paths.pop() {
        let k = (at.head, at.prev);
        if seen.contains(&k) {
            continue;
        }

        seen.insert(k);

        if at.target == at.head {
            return at.cost_so_far;
        }

        paths.extend(at.next(&grid));
    }

    panic!("no place to go")
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct BonusPath {
    target: (usize, usize),
    head: (usize, usize),
    prev: [Dir; 10], // [last, before that, etc..]
    cost_so_far: usize,
    // path: Vec<(usize, usize)>, // for debugging
}

impl BonusPath {
    fn manhattan_to_target(&self) -> usize {
        let (tx, ty) = self.target;
        let (x, y) = self.head;

        (tx - x) + (ty - y)
    }

    fn score(&self) -> usize {
        self.cost_so_far + self.manhattan_to_target()
    }

    fn reached_target(&self) -> bool {
        self.head == self.target
            && self.prev[1..4]
                .iter()
                .all(|&d| d == self.prev[0] || d == NOOP)
    }

    fn next(&self, grid: &Grid) -> Vec<BonusPath> {
        let mut next = vec![];

        for dir in [UP, RIGHT, DOWN, LEFT] {
            if opposite(dir) == self.prev[0] {
                // can't reverse
                continue;
            }

            if self.prev.iter().all(|&d| d == dir) {
                // cannot go in this diration any longer
                continue;
            }

            if self.prev[0] != NOOP
                && self.prev[0] != dir
                && !self.prev[1..4]
                    .iter()
                    .all(|&d| d == self.prev[0] || d == NOOP)
            {
                // cannot change direction yet
                continue;
            }

            let (x, y) = step(self.head, dir);
            if grid[y][x] >= 10 {
                // stay in bounds
                continue;
            }

            let mut prev = self.prev.clone();
            prev.rotate_right(1);
            prev[0] = dir;

            // let mut path = self.path.clone();
            // path.push((x, y));

            next.push(BonusPath {
                target: self.target,
                head: (x, y),
                prev,
                cost_so_far: self.cost_so_far + grid[y][x],
                // path,
            });
        }

        next
    }
}

impl PartialOrd for BonusPath {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(&other))
    }
}

impl Ord for BonusPath {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.score().cmp(&other.score()).reverse()
    }
}

fn bonus(input: &str) -> usize {
    let grid = get_padded_grid(input);
    let h = grid.len();
    let w = grid[0].len();

    let mut seen = HashSet::new();

    let mut paths = BinaryHeap::new();
    paths.push(BonusPath {
        target: (w - 2, h - 2),
        head: (1, 1),
        prev: [NOOP; 10],
        cost_so_far: 0,
        // path: vec![(1, 1)],
    });

    while let Some(at) = paths.pop() {
        let k = (at.head, at.prev);
        if seen.contains(&k) {
            continue;
        }

        seen.insert(k);

        if at.reached_target() {
            return at.cost_so_far;
        }

        paths.extend(at.next(&grid));
    }

    panic!("no place to go")
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
2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533
    "
            .trim(),
        ),
        102
    );

    assert_eq!(
        bonus(
            "
111111111111
999999999991
999999999991
999999999991
999999999991
    "
            .trim(),
        ),
        71
    );
}
