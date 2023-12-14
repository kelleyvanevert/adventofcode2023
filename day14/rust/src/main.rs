use std::time::Instant;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // <1ms
        println!("First part: {}", solve(input));
    });

    time(|| {
        // ±250s
        println!("Bonus: {}", bonus(input));
    });
}

type Grid = Vec<Vec<char>>;

#[allow(unused)]
fn viz(grid: &Grid) {
    println!(
        "===\n{}",
        grid.iter()
            .map(|line| line
                .iter()
                .map(|c| c.to_string())
                .collect::<Vec<_>>()
                .join(""))
            .collect::<Vec<_>>()
            .join("\n")
    );
}

fn tilt_north(grid: &mut Grid) {
    let h = grid.len();
    let w = grid[0].len();

    for x in 0..w {
        let mut num_empty = 0;
        for y in 0..h {
            if grid[y][x] == '.' {
                num_empty += 1;
            } else if grid[y][x] == '#' {
                num_empty = 0;
            } else if grid[y][x] == 'O' && num_empty > 0 {
                grid[y][x] = '.';
                grid[y - num_empty][x] = 'O';
            }
        }
    }
}

fn tilt_south(grid: &mut Grid) {
    let h = grid.len();
    let w = grid[0].len();

    for x in 0..w {
        let mut num_empty = 0;
        for y in (0..h).rev() {
            if grid[y][x] == '.' {
                num_empty += 1;
            } else if grid[y][x] == '#' {
                num_empty = 0;
            } else if grid[y][x] == 'O' && num_empty > 0 {
                grid[y][x] = '.';
                grid[y + num_empty][x] = 'O';
            }
        }
    }
}

fn tilt_west(grid: &mut Grid) {
    let h = grid.len();
    let w = grid[0].len();

    for y in 0..h {
        let mut num_empty = 0;
        for x in 0..w {
            if grid[y][x] == '.' {
                num_empty += 1;
            } else if grid[y][x] == '#' {
                num_empty = 0;
            } else if grid[y][x] == 'O' && num_empty > 0 {
                grid[y][x] = '.';
                grid[y][x - num_empty] = 'O';
            }
        }
    }
}

fn tilt_east(grid: &mut Grid) {
    let h = grid.len();
    let w = grid[0].len();

    for y in 0..h {
        let mut num_empty = 0;
        for x in (0..w).rev() {
            if grid[y][x] == '.' {
                num_empty += 1;
            } else if grid[y][x] == '#' {
                num_empty = 0;
            } else if grid[y][x] == 'O' && num_empty > 0 {
                grid[y][x] = '.';
                grid[y][x + num_empty] = 'O';
            }
        }
    }
}

fn cycle(grid: &mut Grid) {
    tilt_north(grid);
    tilt_west(grid);
    tilt_south(grid);
    tilt_east(grid);
}

fn compute_load(grid: &Grid) -> usize {
    grid.iter()
        .rev()
        .enumerate()
        .map(|(y, row)| (y + 1) * row.iter().filter(|&c| c == &'O').count())
        .sum()
}

fn solve(input: &str) -> usize {
    let mut grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    tilt_north(&mut grid);

    compute_load(&grid)
}

fn bonus(input: &str) -> usize {
    let mut grid = input
        .lines()
        .map(|line| line.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();

    let mut hs = vec![];
    let n = 1000000000;

    for i in 0.. {
        //println!(" - it {i}");
        cycle(&mut grid);
        hs.push(grid.clone());
        //
        if let Some(cycle_length) = (1..i).find(|j| hs[i] == hs[i - j]) {
            //println!("FOUND CYCLE! len={cycle_length}, i={i}");
            let left = n - i;
            let remaining = left % cycle_length - 1;
            //println!("FOUND CYCLE! len={cycle_length}, left={left}, remaining={remaining}");
            for _ in 0..remaining {
                //println!(" - remaining it {k}");
                cycle(&mut grid);
            }
            break;
        }
    }

    compute_load(&grid)
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
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
    "
            .trim()
        ),
        136
    );

    assert_eq!(
        bonus(
            "
O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
    "
            .trim()
        ),
        64
    );
}
