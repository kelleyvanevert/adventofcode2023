use core::panic;
use std::{cmp::Reverse, time::Instant};

use fxhash::{FxHashMap, FxHashSet};
use tuple::Map;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // 374 is too LOW
        // 505 is too LOW
        // 700 is too HIGH
        println!("First part: {}", solve(input));
    });

    // time(|| {

    //     println!("Bonus: {}", bonus(input));
    // });
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
type Vec3 = (i32, i32, i32);

#[derive(Debug, Clone)]
struct Brick {
    i: usize,
    ymin: i32,
    ymax: i32,
    // orientation: Orientation,
    xys: Vec<Vec2>,
}

impl Brick {
    fn is_above(&self, other: &Brick) -> Option<i32> {
        if self.ymin > other.ymax && self.xys.iter().any(|xy| other.xys.contains(xy)) {
            Some(self.ymin - other.ymax - 1)
        } else {
            None
        }
    }

    fn fall(&mut self, dist: i32) {
        self.ymin -= dist;
        self.ymax -= dist;
    }
}

fn solve(input: &str) -> usize {
    let mut bricks = input
        .trim()
        .lines()
        .enumerate()
        .map(|(i, line)| {
            let (a, b) = line.split_once("~").unwrap().map(|s| {
                let cs = s
                    .split(",")
                    .map(|s| s.parse::<i32>().unwrap())
                    .collect::<Vec<i32>>();

                (cs[0], cs[1], cs[2])
            });

            let xys = if a.0 != b.0 {
                (a.0..=b.0).map(|x| (x, a.1)).collect::<Vec<_>>()
            } else if a.1 != b.1 {
                (a.1..=b.1).map(|y| (a.0, y)).collect::<Vec<_>>()
            } else {
                vec![(a.0, a.1)]
            };

            Brick {
                i,
                ymin: a.2,
                ymax: b.2,
                // orientation,
                xys,
            }
        })
        .collect::<Vec<_>>();

    // // first pass: sort from low to high
    // // ===
    // bricks.sort_by_key(|brick| brick.ymin);

    // // second pass: find all bricks below/above each other on any (x,y) pair + distance
    // let mut belows = vec![vec![]; bricks.len()];
    // for above in &bricks {
    //     belows[above.i] = above
    //         .xys
    //         .iter()
    //         .filter_map(|xy| {
    //             if let Some((below, amount)) = bricks
    //                 .iter()
    //                 .filter_map(|below| {
    //                     if below.i != above.i && above.ymin > below.ymax && below.xys.contains(xy) {
    //                         Some((below, above.ymin - below.ymax - 1))
    //                     } else {
    //                         None
    //                     }
    //                 })
    //                 .max_by_key(|(below, _)| below.ymax)
    //             {
    //                 // println!("brick {:?} below {:?} by {}", below, above, amount);
    //                 Some(below.i)
    //             } else {
    //                 None
    //             }
    //         })
    //         .collect::<Vec<_>>();
    // }
    // // println!("BELOWS: {:?}", belows);
    // let is_above = |i: usize, j: usize| belows[i].contains(&j);

    // third pass: fall
    // ===
    let mut falling = (0..bricks.len()).rev().collect::<Vec<_>>();
    loop {
        falling.sort_by_key(|&i| Reverse(bricks[i].ymin));
        let Some(i) = falling.pop() else {
            break;
        };

        // println!("Considering {:?}...", bricks[i]);

        let mut max_fall_dist = bricks[i].ymin - 1;

        for j in 0..bricks.len() {
            if i == j {
                continue;
            }

            if let Some(dist) = bricks[i].is_above(&bricks[j]) {
                max_fall_dist = max_fall_dist.min(dist);
            }
        }

        // let fall_dist = belows[i]
        //     .iter()
        //     .map(|&j| bricks[i].ymin - bricks[j].b.2 - 1)
        //     .min()
        //     .unwrap_or(bricks[i].ymin - 1);

        // println!("  fall dist: {fall_dist}");
        bricks[i].fall(max_fall_dist);
    }

    // println!("FINAL:");
    // for brick in &bricks {
    //     println!("{:?} ", brick);
    // }

    // finally: detect support
    // ===
    let mut supporting = vec![vec![false; bricks.len()]; bricks.len()];
    for i in 0..bricks.len() {
        for j in 0..bricks.len() {
            if i != j && bricks[i].is_above(&bricks[j]).is_some_and(|n| n == 0) {
                supporting[j][i] = true;
            }
        }
        // for &j in belows[i]
        //     .iter()
        //     .filter(|&&j| bricks[i].ymin - bricks[j].b.2 - 1 == 0)
        // {
        //     // j supports i
        //     supporting[j][i] = true;
        //     // println!("brick {:?} supports {:?}", bricks[j], bricks[i]);
        // }
    }

    let result = (0..bricks.len())
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
        .count();

    return result;

    // // third pass: fall, then detect support
    // // ===
    // let mut falling = true;
    // let mut supported_by = vec![FxHashSet::default(); bricks.len()];
    // let mut supporting = vec![FxHashSet::default(); bricks.len()];
    // while falling {
    //     supported_by = vec![FxHashSet::default(); bricks.len()];
    //     supporting = vec![FxHashSet::default(); bricks.len()];
    //     falling = false;

    //     for i in 0..bricks.len() {
    //         println!("considering brick {:?}...", bricks[i]);

    //         let max_fall_dist = belows[i]
    //             .iter()
    //             .map(|&j| bricks[i].ymin - bricks[j].b.2 - 1)
    //             .min()
    //             .unwrap_or(bricks[i].ymin - 1);

    //         bricks[i].fall(max_fall_dist);

    //         println!("  max fall: {}", max_fall_dist);
    //         if max_fall_dist > 0 {
    //             falling = true;
    //         }

    //         supported_by[i] = belows[i]
    //             .iter()
    //             .filter(|&&j| bricks[i].ymin - bricks[j].b.2 - 1 == 0)
    //             .collect();

    //         for &&j in &supported_by[i] {
    //             supporting[j].insert(i);
    //         }
    //     }
    //     println!("After fall:");
    //     for brick in &bricks {
    //         println!(
    //             "{:?}, supporting {:?}, supported by {:?}",
    //             brick, supporting[brick.i], supported_by[brick.i]
    //         );
    //     }
    // }

    // (0..bricks.len())
    //     .filter(|&i| supporting[i].iter().all(|&j| supported_by[j].len() >= 2))
    //     .count()

    0
}

fn bonus(input: &str) -> usize {
    0
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
}
