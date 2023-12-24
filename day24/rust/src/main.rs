use std::time::Instant;

use z3::ast::{Ast, Int};
use z3::*;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // 12783
        // < 1ms
        println!(
            "First part: {}",
            solve(input, 200000000000000.0, 400000000000000.0)
        );
    });

    time(|| {
        // ±2.2s
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

fn solve(input: &str, min: f64, max: f64) -> usize {
    let parse_vec = |s: &str| {
        let mut it = s.split(",").map(|s| s.trim().parse::<f64>().unwrap());
        (it.next().unwrap(), it.next().unwrap(), it.next().unwrap())
    };

    let hailstones = input
        .trim()
        .lines()
        .map(|line| {
            let (pos, vel) = line.split_once(" @ ").unwrap();
            (parse_vec(pos), parse_vec(vel))
        })
        .collect::<Vec<_>>();

    let mut num_found = 0;
    // let mut max_y_off = 0.0f64;

    for i in 0..hailstones.len() {
        for j in (i + 1)..hailstones.len() {
            // println!("{i}, {j}");
            let ((pxi, pyi, _), (vxi, vyi, _)) = hailstones[i];
            let ai = vyi / vxi;
            let bi = pyi - (pxi * ai);

            let ((pxj, pyj, _), (vxj, vyj, _)) = hailstones[j];
            let aj = vyj / vxj;
            let bj = pyj - (pxj * aj);

            if aj - ai == 0.0 {
                // parallel
            } else {
                let x = (bi - bj) / (aj - ai);
                let yi = ai * x + bi;
                let yj = aj * x + bj;
                let ymax = yi.max(yj);
                let ymin = yi.min(yj);
                // if (y - aj * x + bj) < 0.00000001 {
                //     // println!("ij = {i},{i}");
                //     assert_eq!(y, aj * x + bj);
                // }
                // max_y_off = max_y_off.max(ymax - ymin);

                if min <= x && x <= max && min <= ymax && ymin <= max {
                    if vxi >= 0.0 && x >= pxi || vxi <= 0.0 && x <= pxi {
                        if vyi >= 0.0 && ymax >= pyi || vyi <= 0.0 && ymin <= pyi {
                            if vxj >= 0.0 && x >= pxj || vxj <= 0.0 && x <= pxj {
                                if vyj >= 0.0 && ymax >= pyj || vyj <= 0.0 && ymin <= pyj {
                                    // println!("found intersection");
                                    num_found += 1;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // println!("max_y_off = {max_y_off}");

    num_found
}

fn bonus(input: &str) -> usize {
    let parse_vec = |s: &str| {
        let mut it = s.split(",").map(|s| s.trim().parse::<i64>().unwrap());
        (it.next().unwrap(), it.next().unwrap(), it.next().unwrap())
    };

    let hailstones = input
        .trim()
        .lines()
        .map(|line| {
            let (pos, vel) = line.split_once(" @ ").unwrap();
            (parse_vec(pos), parse_vec(vel))
        })
        .collect::<Vec<_>>();

    let cfg = Config::new();
    let ctx = Context::new(&cfg);
    let solver = Solver::new(&ctx);

    let px = Int::new_const(&ctx, "px");
    let py = Int::new_const(&ctx, "py");
    let pz = Int::new_const(&ctx, "pz");
    let vx = Int::new_const(&ctx, "vx");
    let vy = Int::new_const(&ctx, "vy");
    let vz = Int::new_const(&ctx, "vz");

    for (pos, vel) in hailstones {
        let pxi = Int::from_i64(&ctx, pos.0);
        let pyi = Int::from_i64(&ctx, pos.1);
        let pzi = Int::from_i64(&ctx, pos.2);
        let vxi = Int::from_i64(&ctx, vel.0);
        let vyi = Int::from_i64(&ctx, vel.1);
        let vzi = Int::from_i64(&ctx, vel.2);

        let ti = Int::fresh_const(&ctx, "t");

        solver.assert(&(&pxi + &vxi * &ti)._eq(&(&px + &vx * &ti)));
        solver.assert(&(&pyi + &vyi * &ti)._eq(&(&py + &vy * &ti)));
        solver.assert(&(&pzi + &vzi * &ti)._eq(&(&pz + &vz * &ti)));
    }

    assert_eq!(solver.check(), SatResult::Sat);

    let model = solver.get_model().unwrap();

    let result = model.eval(&(px + py + pz), true).unwrap();

    result.as_i64().unwrap() as usize
}

#[test]
fn test() {
    let example_input = "
19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3
";

    assert_eq!(solve(example_input, 7.0, 27.0), 2);

    assert_eq!(bonus(example_input), 47)
}
