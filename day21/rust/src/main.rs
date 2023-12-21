use std::time::Instant;

use fxhash::FxHashSet;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        println!("First part: {}", solve(input, 64));
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

fn solve(input: &str, steps: usize) -> usize {
    let grid: Vec<Vec<char>> = input
        .trim()
        .lines()
        .map(str::chars)
        .map(Iterator::collect)
        .collect();

    let start = grid
        .iter()
        .enumerate()
        .find_map(|(y, line)| {
            line.iter()
                .enumerate()
                .find_map(|(x, c)| (c == &'S').then_some((x as i32, y as i32)))
        })
        .unwrap();

    let get = |(x, y): (i32, i32)| grid.get(y as usize).and_then(|row| row.get(x as usize));

    let mut boundary = FxHashSet::from_iter([start]);

    for _ in 0..steps {
        boundary = boundary
            .into_iter()
            .flat_map(|(x, y)| {
                [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
                    .into_iter()
                    .filter(|p| get(*p).is_some_and(|c| c != &'#'))
            })
            .collect();

        // println!("{boundary:?}");
    }

    boundary.len()
}

fn bonus(input: &str) -> usize {
    0
}

#[test]
fn test() {
    assert_eq!(
        solve(
            "
...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........
",
            6,
        ),
        16
    );
}
