use std::{
    collections::{HashMap, HashSet},
    iter::once,
    time::Instant,
};

fn main() {
    let input = include_str!("../../input.txt");

    //time(|| {
    //    // <1ms
    //    println!("First part: {}", solve(input));
    //});

    time(|| {
        // ±8s
        println!("Bonus: {}", bonus(input));
    });
}

type Grid<T> = Vec<Vec<T>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Dir {
    Up,
    Right,
    Down,
    Left,
}

type Pos = (usize, usize);

type Beam = (Pos, Dir);

#[allow(unused)]
fn viz(grid: &Grid<impl ToString>) {
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

fn get_padded_grid(input: &str) -> Grid<char> {
    let w = input.lines().next().unwrap().len();

    once(vec!['X'; w + 2])
        .chain(
            input
                .lines()
                .map(|line| {
                    let mut row = line.chars().collect::<Vec<_>>();
                    row.insert(0, 'X');
                    row.push('X');
                    row
                })
                .chain(once(vec!['X'; w + 2])),
        )
        .collect::<Vec<_>>()
}

fn next(x: usize, y: usize, dir: Dir, c: char) -> Vec<Beam> {
    match c {
        '.' => match dir {
            Dir::Up => vec![((x, y - 1), Dir::Up)],
            Dir::Right => vec![((x + 1, y), Dir::Right)],
            Dir::Down => vec![((x, y + 1), Dir::Down)],
            Dir::Left => vec![((x - 1, y), Dir::Left)],
        },
        '\\' => match dir {
            Dir::Up => vec![((x - 1, y), Dir::Left)],
            Dir::Right => vec![((x, y + 1), Dir::Down)],
            Dir::Down => vec![((x + 1, y), Dir::Right)],
            Dir::Left => vec![((x, y - 1), Dir::Up)],
        },
        '/' => match dir {
            Dir::Up => vec![((x + 1, y), Dir::Right)],
            Dir::Right => vec![((x, y - 1), Dir::Up)],
            Dir::Down => vec![((x - 1, y), Dir::Left)],
            Dir::Left => vec![((x, y + 1), Dir::Down)],
        },
        '|' => match dir {
            Dir::Up => vec![((x, y - 1), Dir::Up)],
            Dir::Down => vec![((x, y + 1), Dir::Down)],
            Dir::Right | Dir::Left => {
                vec![((x, y - 1), Dir::Up), ((x, y + 1), Dir::Down)]
            }
        },
        '-' => match dir {
            Dir::Right => vec![((x + 1, y), Dir::Right)],
            Dir::Left => vec![((x - 1, y), Dir::Left)],
            Dir::Up | Dir::Down => {
                vec![((x - 1, y), Dir::Left), ((x + 1, y), Dir::Right)]
            }
        },
        'X' => vec![],
        _ => unreachable!(),
    }
}

struct Context {
    grid: Grid<char>,
    graph: HashMap<Beam, Vec<Beam>>,
    gathered: HashMap<Beam, HashSet<Beam>>,
}

impl Context {
    fn new(grid: Grid<char>) -> Self {
        Self {
            grid,
            graph: HashMap::new(),
            gathered: HashMap::new(),
        }
    }

    // why is this so hard? 😅
    // ===
    // fn gather(&mut self, mut prev: Vec<Beam>, at: Beam) -> HashSet<Beam> {
    //     println!("gather {at:?}...");

    //     // caching
    //     if let Some(res) = self.gathered.get(&at) {
    //         return res.clone();
    //     }

    //     if self.graph[&at].len() == 0 {
    //         let res = HashSet::new();
    //         self.gathered.insert(at, res.clone());
    //         return res;
    //     }

    //     if prev.contains(&at) {
    //         // there's a cycle
    //         let mut todo = vec![at];
    //         let mut collected = HashSet::new();

    //         while let Some(beam) = todo.pop() {
    //             if !collected.contains(&beam) {
    //                 collected.insert(beam);
    //                 todo.extend(self.graph[&beam].clone());
    //             }
    //         }

    //         self.gathered.insert(at, collected.clone());
    //         return collected.clone();
    //     }

    //     prev.push(at);

    //     let res = self.graph[&at]
    //         .clone()
    //         .into_iter()
    //         .map(|b| self.gather(prev.clone(), b))
    //         .flatten()
    //         .collect::<HashSet<_>>();

    //     self.gathered.insert(at, res.clone());
    //     res
    // }
}

fn bonus(input: &str) -> usize {
    let grid = get_padded_grid(input);
    let h = grid.len();
    let w = grid[0].len();

    let mut ctx = Context::new(grid);

    let mut entries = vec![];
    entries.extend((1..(h - 1)).flat_map(|y| [((1, y), Dir::Right), ((w - 2, y), Dir::Left)]));
    entries.extend((1..(w - 1)).flat_map(|x| [((x, 1), Dir::Down), ((x, h - 2), Dir::Up)]));

    // step 1. traverse all paths from entry points
    {
        let mut todo = entries.clone();
        while let Some(((x, y), dir)) = todo.pop() {
            if !ctx.graph.contains_key(&((x, y), dir)) {
                let ns = next(x, y, dir, ctx.grid[y][x]);
                ctx.graph.insert(((x, y), dir), ns.clone());
                // println!("{:?} -> {:?}", ((x, y), dir), ns);
                todo.extend(ns);
            }
        }
    }

    // step 2. round up
    // let mut gathered = HashMap::new();
    let gather = |start: Beam| {
        println!("gather {start:?}...");

        let mut todo = vec![start];
        let mut collected = vec![];

        while let Some(beam) = todo.pop() {
            if !collected.contains(&beam) {
                collected.push(beam);
                todo.extend(ctx.graph[&beam].clone());
            }
        }

        // found.iter().sum::<usize>()
        collected
            .into_iter()
            .map(|(pos, _)| pos)
            .filter(|&(x, y)| ctx.grid[y][x] != 'X')
            .collect::<HashSet<_>>()
            .len()
    };

    // println!("{:?}", gather(((1, 1), Dir::Right)));
    entries.into_iter().map(gather).max().unwrap()
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
        bonus(
            r"
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
    "
            .trim()
        ),
        51
    );
}
