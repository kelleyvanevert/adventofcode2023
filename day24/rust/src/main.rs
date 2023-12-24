use std::time::Instant;

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

    // time(|| {
    //     // ±1.5s with par_iter in discover_bonus
    //     // ±8s without par_iter in discover_bonus
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

    for i in 0..hailstones.len() {
        for j in (i + 1)..hailstones.len() {
            // println!("{i}, {j}");
            let ((x1i, y1i, _), (dxi, dyi, _)) = hailstones[i];
            let ai = dyi / dxi;
            let bi = y1i - (x1i * ai);

            let ((x1j, y1j, _), (dxj, dyj, _)) = hailstones[j];
            let aj = dyj / dxj;
            let bj = y1j - (x1j * aj);

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

                if min <= x && x <= max && min <= ymax && ymin <= max {
                    if dxi >= 0.0 && x >= x1i || dxi <= 0.0 && x <= x1i {
                        if dyi >= 0.0 && ymax >= y1i || dyi <= 0.0 && ymin <= y1i {
                            if dxj >= 0.0 && x >= x1j || dxj <= 0.0 && x <= x1j {
                                if dyj >= 0.0 && ymax >= y1j || dyj <= 0.0 && ymin <= y1j {
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

    num_found
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
}
