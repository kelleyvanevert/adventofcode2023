use std::{collections::VecDeque, time::Instant};

use fxhash::FxHashSet;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±5ms
        println!("First part: {}", solve(input, 64));
    });

    time(|| {
        // 2565759115453668192 is too HIGH
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

fn fill(grid: &Vec<Vec<char>>, start: (i32, i32), steps: usize) -> usize {
    let get = |(x, y): (i32, i32)| grid.get(y as usize).and_then(|row| row.get(x as usize));

    let mut found = FxHashSet::default();
    let mut seen = FxHashSet::from_iter([start]);
    let mut todo = VecDeque::from_iter([(start, steps)]);

    while let Some(((x, y), steps)) = todo.pop_front() {
        if steps % 2 == 0 {
            found.insert((x, y));
        }

        if steps == 0 {
            continue;
        }

        for p in [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)] {
            if get(p).is_some_and(|c| c != &'#') && !seen.contains(&p) {
                seen.insert(p);
                todo.push_back((p, steps - 1));
            }
        }
    }

    found.len()
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

    fill(&grid, start, steps)
}

fn bonus(input: &str) -> usize {
    let grid: Vec<Vec<char>> = input
        .trim()
        .lines()
        .map(str::chars)
        .map(Iterator::collect)
        .collect();

    let (sx, sy) = grid
        .iter()
        .enumerate()
        .find_map(|(y, line)| {
            line.iter()
                .enumerate()
                .find_map(|(x, c)| (c == &'S').then_some((x as i32, y as i32)))
        })
        .unwrap();

    let steps = 26501365;

    let size = grid.len();

    assert_eq!(grid[0].len(), size);
    assert_eq!(sx, size as i32 / 2);
    assert_eq!(sy, size as i32 / 2);
    assert_eq!(steps % size, size / 2);

    let grid_width = steps / size - 1;

    let num_odd = (grid_width / 2 * 2 + 1).pow(2);
    let num_even = ((grid_width + 1) / 2 * 2).pow(2);

    let odd_points = fill(&grid, (sx, sy), size * 2 + 1);
    let even_points = fill(&grid, (sx, sy), size * 2);

    let corner_t = fill(&grid, (sx, size as i32 - 1), size - 1);
    let corner_r = fill(&grid, (0, sy), size - 1);
    let corner_b = fill(&grid, (sx, 0), size - 1);
    let corner_l = fill(&grid, (size as i32 - 1, sy), size - 1);

    let small_tr = fill(&grid, (0, size as i32 - 1), size / 2 - 1);
    let small_tl = fill(&grid, (size as i32 - 1, size as i32 - 1), size / 2 - 1);
    let small_br = fill(&grid, (0, 0), size / 2 - 1);
    let small_bl = fill(&grid, (size as i32 - 1, 0), size / 2 - 1);

    let large_tr = fill(&grid, (0, size as i32 - 1), size * 3 / 2 - 1);
    let large_tl = fill(&grid, (size as i32 - 1, size as i32 - 1), size * 3 / 2 - 1);
    let large_br = fill(&grid, (0, 0), size * 3 / 2 - 1);
    let large_bl = fill(&grid, (size as i32 - 1, 0), size * 3 / 2 - 1);

    num_odd * odd_points
        + num_even * even_points
        + corner_t
        + corner_r
        + corner_b
        + corner_l
        + (grid_width + 1) * (small_tr + small_tl + small_br + small_bl)
        + grid_width * (large_tr + large_tl + large_br + large_bl)
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
