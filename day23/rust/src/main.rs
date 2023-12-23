use std::time::Instant;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±550ms
        println!("First part: {}", solve(input));
    });

    // time(|| {
    //     // ±350ms
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

fn get(grid: &Vec<Vec<char>>, (x, y): (i32, i32)) -> Option<char> {
    (x >= 0 && y >= 0)
        .then(|| grid.get(y as usize).and_then(|row| row.get(x as usize)))
        .flatten()
        .copied()
}

fn added(prev: &Vec<(i32, i32)>, next: (i32, i32)) -> Vec<(i32, i32)> {
    let mut prev = prev.clone();
    prev.push(next);
    prev
}

fn discover(
    grid: &Vec<Vec<char>>,
    end: (i32, i32),
    prev: Vec<(i32, i32)>,
    (x, y): (i32, i32),
) -> Option<usize> {
    if (x, y) == end {
        return Some(0);
    }

    if prev.contains(&(x, y)) {
        return None;
    }

    match get(grid, (x, y)) {
        Some('>') => return discover(grid, end, added(&prev, (x, y)), (x + 1, y)).map(|n| n + 1),
        Some('<') => return discover(grid, end, added(&prev, (x, y)), (x - 1, y)).map(|n| n + 1),
        Some('v') => return discover(grid, end, added(&prev, (x, y)), (x, y + 1)).map(|n| n + 1),
        Some('^') => return discover(grid, end, added(&prev, (x, y)), (x, y - 1)).map(|n| n + 1),
        _ => {}
    }

    [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
        .iter()
        .filter_map(|&n| match get(grid, n) {
            Some(c) if c != '#' => discover(grid, end, added(&prev, (x, y)), n),
            _ => None,
        })
        .map(|n| n + 1)
        .max()
}

fn solve(input: &str) -> usize {
    let grid: Vec<Vec<char>> = input
        .trim()
        .lines()
        .map(str::chars)
        .map(Iterator::collect)
        .collect();

    let h = grid.len() as i32;
    let w = grid[0].len() as i32;

    let start = (1, 0);

    let end = (w - 2, h - 1);

    discover(&grid, end, vec![], start).unwrap()
}

#[test]
fn test() {
    let example_input = "
#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#
    ";

    assert_eq!(solve(example_input), 94);
}
