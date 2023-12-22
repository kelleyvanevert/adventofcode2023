#![feature(let_chains)]

use std::{cmp::Reverse, time::Instant, vec};

use tuple::Map;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±40ms
        println!("First part: {}", solve(input));
    });

    time(|| {
        // ±350ms
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

type Vec2 = (i32, i32);

#[derive(Debug, Clone)]
struct Brick {
    zmin: i32,
    zmax: i32,
    xys: Vec<Vec2>,
}

impl Brick {
    fn is_above(&self, other: &Brick) -> Option<i32> {
        if self.zmin > other.zmax && self.xys.iter().any(|xy| other.xys.contains(xy)) {
            Some(self.zmin - other.zmax - 1)
        } else {
            None
        }
    }

    fn fall(&mut self, dist: i32) {
        self.zmin -= dist;
        self.zmax -= dist;
    }
}

fn parse(input: &str) -> Vec<Brick> {
    input
        .trim()
        .lines()
        .map(|line| {
            let ((xmin, ymin, zmin), (xmax, ymax, zmax)) = line.split_once("~").unwrap().map(|s| {
                let cs = s
                    .split(",")
                    .map(|s| s.parse::<i32>().unwrap())
                    .collect::<Vec<i32>>();

                (cs[0], cs[1], cs[2])
            });

            Brick {
                zmin,
                zmax,
                xys: if xmin != xmax {
                    (xmin..=xmax).map(|x| (x, ymin)).collect::<Vec<_>>()
                } else if ymin != ymax {
                    (ymin..=ymax).map(|y| (xmin, y)).collect::<Vec<_>>()
                } else {
                    vec![(xmin, ymin)]
                },
            }
        })
        .collect::<Vec<_>>()
}

fn simulate_fall(bricks: &mut Vec<Brick>) {
    let mut falling = Vec::from_iter(0..bricks.len());

    while let Some(i) = {
        falling.sort_by_key(|&i| Reverse(bricks[i].zmin));
        falling.pop()
    } {
        let mut max_fall_dist = bricks[i].zmin - 1;

        for j in 0..bricks.len() {
            if i != j && let Some(dist) = bricks[i].is_above(&bricks[j]) {
                max_fall_dist = max_fall_dist.min(dist);
            }
        }

        bricks[i].fall(max_fall_dist);
    }
}

fn detect_support(bricks: &Vec<Brick>) -> Vec<Vec<bool>> {
    let mut supporting = vec![vec![false; bricks.len()]; bricks.len()];

    for i in 0..bricks.len() {
        for j in 0..bricks.len() {
            if i != j && bricks[i].is_above(&bricks[j]).is_some_and(|n| n == 0) {
                supporting[j][i] = true;
            }
        }
    }

    supporting
}

fn solve(input: &str) -> usize {
    let mut bricks = parse(input);
    simulate_fall(&mut bricks);
    let supporting = detect_support(&bricks);

    (0..bricks.len())
        .filter(|&i| {
            supporting[i].iter().enumerate().all(|(j, is_supporting)| {
                if !is_supporting {
                    return true; // vacuously
                }

                // i supports j
                // want to know: that j can also be supported by someone else
                (0..bricks.len()).any(|other_i| i != other_i && supporting[other_i][j])
            })
        })
        .count()
}

fn simulate_chain_reaction(
    bricks: &Vec<Brick>,
    mut supporting: Vec<Vec<bool>>,
    start_with: usize,
) -> usize {
    let mut remove = vec![start_with];
    let mut num_fall = 0;

    while let Some(i) = remove.pop() {
        for j in 0..bricks.len() {
            if i != j && supporting[i][j] {
                supporting[i][j] = false;

                // only if j is not supported any more!
                if !(0..bricks.len()).any(|i| supporting[i][j]) {
                    remove.push(j);
                    num_fall += 1;
                }
            }
        }
    }

    num_fall
}

fn bonus(input: &str) -> usize {
    let mut bricks = parse(input);
    simulate_fall(&mut bricks);
    let supporting = detect_support(&bricks);

    (0..bricks.len())
        .into_iter()
        .map(|i| simulate_chain_reaction(&bricks, supporting.clone(), i))
        .sum()
}

#[test]
fn test() {
    assert_eq!(
        solve(
            "
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
",
        ),
        5
    );

    assert_eq!(
        bonus(
            "
1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9
",
        ),
        7
    );
}
