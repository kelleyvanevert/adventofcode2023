use std::{iter::once, time::Instant};

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±80ms
        let res = solve(input);
        println!("First part: {}", res.0);
        println!("Bonus: {}", res.1);
        assert_eq!(res, (6842, 393));
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

type Pos = (i64, i64);

fn is_clockwise(polygon: &Vec<Pos>) -> bool {
    polygon
        .into_iter()
        .zip(polygon.into_iter().skip(1).chain(once(&polygon[0])))
        .map(|(a, b)| (b.1 - a.1) * (b.0 + a.0))
        .sum::<i64>()
        >= 0
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
enum Dir {
    Up,
    Right,
    Down,
    Left,
}

use Dir::*;

fn solve(input: &str) -> (i64, i64) {
    let grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let start = grid
        .iter()
        .enumerate()
        .find_map(|(y, row)| {
            row.iter().enumerate().find_map(|(x, c)| {
                if *c == 'S' {
                    Some((y as i64, x as i64))
                } else {
                    None
                }
            })
        })
        .unwrap();

    let h = grid.len() as i64;
    let w = grid[0].len() as i64;

    let get = |(y, x): Pos| grid[y as usize][x as usize];

    let neighbors = |p: Pos| {
        [
            (Up, (p.0 - 1, p.1)),
            (Left, (p.0, p.1 - 1)),
            (Down, (p.0 + 1, p.1)),
            (Right, (p.0, p.1 + 1)),
        ]
        .into_iter()
        .filter(|(_, p)| p.0 >= 0 && p.1 >= 0 && p.0 < h - 1 && p.1 < w - 1)
    };

    let next = |prev_dir: Dir, (y, x): Pos| match (prev_dir, get((y, x))) {
        (Up, '|') => Some((Up, (y - 1, x), vec![(y, x + 1)])),
        (Up, '7') => Some((
            Left,
            (y, x - 1),
            vec![(y, x + 1), (y - 1, x), (y - 1, x + 1)],
        )),
        (Up, 'F') => Some((Right, (y, x + 1), vec![])),
        (Right, '-') => Some((Right, (y, x + 1), vec![(y + 1, x)])),
        (Right, '7') => Some((Down, (y + 1, x), vec![])),
        (Right, 'J') => Some((Up, (y - 1, x), vec![(y + 1, x), (y + 1, x + 1), (y, x + 1)])),
        (Down, '|') => Some((Down, (y + 1, x), vec![(y, x - 1)])),
        (Down, 'L') => Some((
            Right,
            (y, x + 1),
            vec![(y, x - 1), (y + 1, x - 1), (y + 1, x)],
        )),
        (Down, 'J') => Some((Left, (y, x - 1), vec![])),
        (Left, '-') => Some((Left, (y, x - 1), vec![(y - 1, x)])),
        (Left, 'L') => Some((Up, (y - 1, x), vec![])),
        (Left, 'F') => Some((
            Down,
            (y + 1, x),
            vec![(y - 1, x), (y - 1, x - 1), (y, x - 1)],
        )),
        _ => None,
    };

    // (on our right-hand side, at least)
    let mut queue = vec![];
    let mut trail = vec![start];

    let (mut dir, mut at) = neighbors(start)
        .find(|(d, p)| next(*d, *p).is_some())
        .expect("first move found");

    let solution = loop {
        if get(at) == 'S' {
            break trail.len() as i64 / 2;
        } else {
            trail.push(at);
            let t = next(dir, at).expect("move possible");
            dir = t.0;
            at = t.1;
            queue.extend(t.2);
        }
    };

    let mut inside = vec![];

    let mut i = 0;
    while let Some(r) = queue.get(i) {
        i += 1;

        if trail.contains(&r) {
            // not inside
        } else if r.0 < 0 || r.1 < 0 || r.0 >= h || r.1 >= w {
            // not in bounds
        } else if inside.contains(r) {
            // already done
        } else {
            inside.push(*r);

            for (_, n) in neighbors(*r) {
                if !queue.contains(&n) && !inside.contains(&n) && !trail.contains(&n) {
                    queue.push(n);
                }
            }
        }
    }

    let mut num_inside = inside.len() as i64;
    if is_clockwise(&trail) {
        num_inside = w * h - num_inside - trail.len() as i64;
    }

    (solution, num_inside)
}

#[test]
fn test() {
    assert_eq!(
        solve(
            "
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
    "
            .trim(),
        ),
        (8, 1)
    );

    assert_eq!(
        solve(
            "
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
    "
            .trim(),
        ),
        (23, 4)
    );

    assert_eq!(
        solve(
            "
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
    "
            .trim(),
        ),
        (70, 8)
    );
}
