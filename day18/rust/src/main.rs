use std::{iter::once, time::Instant};

use fxhash::FxHashSet;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±10ms
        println!("First part: {}", solve(input));
    });

    // time(|| {
    //     // ±4s
    //     println!("Bonus: {}", bonus(input));
    // });
}

type Dir = (i64, i64);
type Pos = (i64, i64);

const UP: Dir = (0, -1);
const RIGHT: Dir = (1, 0);
const DOWN: Dir = (0, 1);
const LEFT: Dir = (-1, 0);

fn parse(input: &str) -> Vec<(Dir, i64)> {
    input
        .lines()
        .map(|line| {
            let (dir, rest) = line.split_once(" ").unwrap();
            let (steps, _color) = rest.split_once(" ").unwrap();
            let steps = steps.parse::<i64>().unwrap();
            let dir = match dir {
                "U" => UP,
                "R" => RIGHT,
                "D" => DOWN,
                "L" => LEFT,
                _ => unreachable!(),
            };
            (dir, steps)
        })
        .collect()
}

fn is_clockwise(polygon: &Vec<Pos>) -> bool {
    polygon
        .into_iter()
        .zip(polygon.into_iter().skip(1).chain(once(&polygon[0])))
        .map(|(a, b)| (b.1 - a.1) * (b.0 + a.0))
        .sum::<i64>()
        >= 0
}

fn solve(input: &str) -> usize {
    let instructions = parse(input);

    // first, trace the whole edge
    let mut edge = vec![];
    let (mut x, mut y) = (0, 0);
    for &((dx, dy), steps) in &instructions {
        for _ in 0..steps {
            x += dx;
            y += dy;
            edge.push((x, y));
        }
    }

    // then, trace the inside of the edge
    let clockwise = is_clockwise(&edge);
    let edge = FxHashSet::from_iter(edge);
    let mut inner_edge = vec![];
    let (mut x, mut y) = (0, 0);
    for &((dx, dy), steps) in &instructions {
        for _ in 0..steps {
            x += dx;
            y += dy;

            let p = if clockwise {
                // right hand side: (-dy, dx)
                (x - dy, y + dx)
            } else {
                // left hand side: (dy, -dx)
                (x + dy, y - dx)
            };

            if !edge.contains(&p) && !inner_edge.contains(&p) {
                inner_edge.push(p);
            }
        }
    }

    // lastly, perform a discovery
    let mut inside = FxHashSet::default();
    let mut todo = inner_edge;

    while let Some((x, y)) = todo.pop() {
        inside.insert((x, y));
        for (dx, dy) in [UP, RIGHT, DOWN, LEFT] {
            let n = (x + dx, y + dy);
            if !edge.contains(&n) && !inside.contains(&n) {
                todo.push(n);
            }
        }
    }

    inside.len() + edge.len()
}

fn bonus(input: &str) -> usize {
    0
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
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
    "
            .trim(),
        ),
        62
    );
}
