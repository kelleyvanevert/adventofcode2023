use std::{iter::once, time::Instant};

use fxhash::FxHashSet;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±10ms
        println!("First part: {}", solve(input));
    });

    time(|| {
        // 9446280843696 is TOO LOW
        println!("Bonus: {}", bonus(input));
    });
}

type Dir = (i64, i64);
type Pos = (i64, i64);

const NOOP: Dir = (0, 0);
const UP: Dir = (0, -1);
const RIGHT: Dir = (1, 0);
const DOWN: Dir = (0, 1);
const LEFT: Dir = (-1, 0);

fn rotate_right((dx, dy): Dir) -> Dir {
    (-dy, dx)
}

fn rotate_left((dx, dy): Dir) -> Dir {
    (dy, -dx)
}

fn add((ax, ay): Pos, (bx, by): Pos) -> Pos {
    (ax + bx, ay + by)
}

fn opposite((dx, dy): Dir) -> Dir {
    (-dx, -dy)
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
    let instructions = input
        .lines()
        .map(|line| {
            let (dir, rest) = line.split_once(" ").unwrap();
            let (steps, _) = rest.split_once(" ").unwrap();
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
        .collect::<Vec<_>>();

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
    // let instructions = input
    //     .lines()
    //     .map(|line| {
    //         let (dir, rest) = line.split_once(" ").unwrap();
    //         let (steps, _) = rest.split_once(" ").unwrap();
    //         let steps = steps.parse::<i64>().unwrap();
    //         let dir = match dir {
    //             "U" => UP,
    //             "R" => RIGHT,
    //             "D" => DOWN,
    //             "L" => LEFT,
    //             _ => unreachable!(),
    //         };
    //         (dir, steps)
    //     })
    //     .collect::<Vec<_>>();

    let instructions = input
        .lines()
        .map(|line| {
            let color = &line.split_once(" ").unwrap().1.split_once(" ").unwrap().1[2..8];
            let steps = i64::from_str_radix(&color[0..5], 16).unwrap();
            let dir = match &color[5..6] {
                "0" => RIGHT,
                "1" => DOWN,
                "2" => LEFT,
                "3" => UP,
                _ => unreachable!(),
            };
            (dir, steps)
        })
        .collect::<Vec<_>>();

    println!("tracing edge corners...");
    let mut edge_corners = vec![];
    let (mut x, mut y) = (0, 0);
    for &((dx, dy), steps) in &instructions {
        x += dx * steps;
        y += dy * steps;
        edge_corners.push((x, y));
        println!("  {:?}", (x, y));
    }

    let mut xs = edge_corners.iter().map(|(x, _)| *x).collect::<Vec<_>>();
    xs.sort();
    xs.dedup();
    println!("xs = {xs:?}");

    let mut ys = edge_corners.iter().map(|(_, y)| *y).collect::<Vec<_>>();
    ys.sort();
    ys.dedup();
    println!("ys = {ys:?}");

    let midway_points = |(prev_x, prev_y): Pos, (next_x, next_y): Pos| {
        let mut midway = vec![];

        if prev_x == next_x {
            // going vertical
            for &y in &ys {
                if prev_y.min(next_y) < y && y < prev_y.max(next_y) {
                    midway.push((next_x, y - 1));
                    midway.push((next_x, y));
                    midway.push((next_x, y + 1));
                }
            }
        } else if prev_y == next_y {
            // going horizontal
            for &x in &xs {
                if prev_x.min(next_x) < x && x < prev_x.max(next_x) {
                    midway.push((x - 1, next_y));
                    midway.push((x, next_y));
                    midway.push((x + 1, next_y));
                }
            }
        } else {
            unreachable!();
        }

        if midway.len() > 0 {
            println!("  added {} midway points", midway.len());
        }

        midway
    };

    println!("tracing inner edge corners...");
    let clockwise = is_clockwise(&edge_corners);
    let mut inner_edge_points = vec![];
    let (mut x, mut y) = (0, 0);
    let mut prev_dir = NOOP;
    for &(dir, steps) in instructions.iter().chain(once(&instructions[0])) {
        let (dx, dy) = dir;
        if prev_dir != NOOP && (rotate_left(prev_dir) == dir) == clockwise {
            // turning left (assuming clockwise)
            let next = add(add((x, y), opposite(prev_dir)), opposite(dir));
            if let Some(&prev) = inner_edge_points.last() {
                inner_edge_points.extend(midway_points(prev, next));
            }
            inner_edge_points.push(next);
            inner_edge_points.push(add((x, y), opposite(dir)));
            inner_edge_points.push(add(add((x, y), prev_dir), opposite(dir)));
            inner_edge_points.push(add((x, y), prev_dir));
            inner_edge_points.push(add(add((x, y), prev_dir), dir));
        } else if prev_dir != NOOP && (rotate_right(prev_dir) == dir) == clockwise {
            // turning right (assuming clockwise)
            let next = add(add((x, y), dir), opposite(prev_dir));
            if let Some(&prev) = inner_edge_points.last() {
                inner_edge_points.extend(midway_points(prev, next));
            }
            inner_edge_points.push(next);
        }

        x += dx * steps;
        y += dy * steps;

        prev_dir = dir;
    }

    // println!("inner edge corners: {inner_edge_corners:?}");

    let inner_edge_points = FxHashSet::from_iter(inner_edge_points);

    let mut chunks = FxHashSet::default();

    for i in 0..xs.len() - 1 {
        'find: for j in 0..ys.len() - 1 {
            // if xs[i + 1] - xs[i] < 2 || ys[j + 1] - ys[j] < 2 {
            //     continue;
            // }

            let (tl_x, tl_y) = (xs[i], ys[j]);
            let (tr_x, tr_y) = (xs[i + 1], ys[j]);
            let (bl_x, bl_y) = (xs[i], ys[j + 1]);
            let (br_x, br_y) = (xs[i + 1], ys[j + 1]);

            #[rustfmt::skip]
            let check_inside = {
                [
                    (tl_x  , tl_y  ), (tl_x+1, tl_y  ),
                    (tl_x  , tl_y+1), (tl_x+1, tl_y+1),

                    (tr_x-1, tr_y  ), (tr_x  , tr_y  ),
                    (tr_x-1, tr_y+1), (tr_x  , tr_y+1),

                    (bl_x  , bl_y-1), (bl_x+1, bl_y-1),
                    (bl_x  , bl_y  ), (bl_x+1, bl_y  ),

                    (br_x-1, br_y-1), (br_x  , br_y-1),
                    (br_x-1, br_y  ), (br_x  , br_y  ),
                ]
            };

            for p in check_inside {
                if inner_edge_points.contains(&p) {
                    // the four corners
                    chunks.insert(((tl_x, tl_y), (tl_x, tl_y)));
                    chunks.insert(((tr_x, tr_y), (tr_x, tr_y)));
                    chunks.insert(((bl_x, bl_y), (bl_x, bl_y)));
                    chunks.insert(((br_x, br_y), (br_x, br_y)));

                    // the top and bottom edges
                    chunks.insert(((tl_x + 1, tl_y), (tr_x - 1, tr_y)));
                    chunks.insert(((bl_x + 1, bl_y), (br_x - 1, br_y)));

                    // left left and right edges
                    chunks.insert(((tl_x, tl_y + 1), (bl_x, bl_y - 1)));
                    chunks.insert(((tr_x, tr_y + 1), (br_x, br_y - 1)));

                    // the inside
                    chunks.insert(((tl_x + 1, tl_y + 1), (br_x - 1, br_y - 1)));

                    // println!("\ncube on inside");
                    // println!("{:?} --- {:?}", (tl_x, tl_y), (tr_x, tr_y));
                    // println!(" |              |",);
                    // println!("{:?} --- {:?}", (bl_x, bl_y), (br_x, br_y));
                    continue 'find;
                }
            }
        }
    }

    let mut surface = 0;

    println!("chunks collected:");
    for ((xmin, ymin), (xmax, ymax)) in chunks {
        let area = (ymax + 1 - ymin).max(0) * (xmax + 1 - xmin).max(0);
        if area >= 1 {
            // println!(
            //     "  X {}--{},  Y {}--{} of size {}",
            //     xmin, xmax, ymin, ymax, area
            // );
        }
        surface += area;
    }

    surface as usize
}

fn time<F>(f: F)
where
    F: FnOnce(),
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

    assert_eq!(
        bonus(
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
        952408144115
    );
}
