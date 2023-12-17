use std::{
    collections::{HashMap, HashSet},
    iter::once,
    time::Instant,
};

use fxhash::{FxBuildHasher, FxHashSet, FxHasher};
use itertools::Itertools;
use strongly::Tarjan;

mod strongly;

fn main() {
    let input = include_str!("../../input.txt");

    //time(|| {
    //    // <1ms
    //    println!("First part: {}", solve(input));
    //});

    time(|| {
        // ±5.5s
        println!("Bonus: {}", bonus(input, false));
    });

    time(|| {
        // ±6.5s
        println!("Bonus: {}", bonus(input, true));
    });
}

type Grid<T> = Vec<Vec<T>>;

type Dir = usize;

const UP: usize = 0;
const RIGHT: usize = 1;
const DOWN: usize = 2;
const LEFT: usize = 3;

type Beam = ((usize, usize), Dir);

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

struct Context {
    grid: Grid<char>,
    h: usize,
    w: usize,
    entries: Vec<usize>,
    adj: Vec<Vec<usize>>,

    comp: Vec<usize>,
    comp_adj: Vec<Vec<usize>>,
    comp_positions: HashMap<usize, FxHashSet<usize>, FxBuildHasher>,
    comp_reach: HashMap<usize, FxHashSet<usize>, FxBuildHasher>,
}

impl Context {
    fn new(grid: Grid<char>) -> Self {
        let h = grid.len();
        let w = grid[0].len();

        Self {
            grid,
            h,
            w,
            entries: vec![],
            adj: vec![vec![]; w * h * 4],

            comp: vec![],
            comp_adj: vec![],
            comp_positions: HashMap::default(),
            comp_reach: HashMap::default(),
        }
    }

    fn enc(&self, x: usize, y: usize, dir: Dir) -> usize {
        (y * self.h + x) * 4 + dir
    }

    fn dec(&self, k: usize) -> Beam {
        let dir = k % 4;
        let x = (k >> 2) % self.h;
        let y = (k >> 2) / self.h;
        ((x, y), dir)
    }

    fn fill_entries(&mut self) {
        let mut entries = vec![];

        entries.extend((1..(self.h - 1)).flat_map(|y| {
            [
                //
                self.enc(1, y, RIGHT),
                self.enc(self.w - 2, y, LEFT),
            ]
        }));

        entries.extend((1..(self.w - 1)).flat_map(|x| {
            [
                //
                self.enc(x, 1, DOWN),
                self.enc(x, self.h - 2, UP),
            ]
        }));

        self.entries = entries;
    }

    fn fill_adj_matrix(&mut self) {
        let mut todo = self.entries.clone();
        let mut done = vec![false; self.w * self.h * 4];
        while let Some(k) = todo.pop() {
            if !done[k] {
                let ((x, y), dir) = self.dec(k);

                let ns = self
                    .next(x, y, dir, self.grid[y][x])
                    .into_iter()
                    .filter(|&((x, y), _)| self.grid[y][x] != 'X')
                    .map(|((x, y), dir)| self.enc(x, y, dir))
                    .collect::<Vec<_>>();

                self.adj[k] = ns.clone();
                done[k] = true;

                // println!("{:?} -> {:?}", ((x, y), dir), ns);
                todo.extend(ns);
            }
        }
    }

    fn next(&self, x: usize, y: usize, dir: Dir, c: char) -> Vec<Beam> {
        match c {
            '.' => match dir {
                UP => vec![((x, y - 1), UP)],
                RIGHT => vec![((x + 1, y), RIGHT)],
                DOWN => vec![((x, y + 1), DOWN)],
                LEFT => vec![((x - 1, y), LEFT)],
                _ => unreachable!(),
            },
            '\\' => match dir {
                UP => vec![((x - 1, y), LEFT)],
                RIGHT => vec![((x, y + 1), DOWN)],
                DOWN => vec![((x + 1, y), RIGHT)],
                LEFT => vec![((x, y - 1), UP)],
                _ => unreachable!(),
            },
            '/' => match dir {
                UP => vec![((x + 1, y), RIGHT)],
                RIGHT => vec![((x, y - 1), UP)],
                DOWN => vec![((x - 1, y), LEFT)],
                LEFT => vec![((x, y + 1), DOWN)],
                _ => unreachable!(),
            },
            '|' => match dir {
                UP => vec![((x, y - 1), UP)],
                DOWN => vec![((x, y + 1), DOWN)],
                RIGHT | LEFT => {
                    vec![((x, y - 1), UP), ((x, y + 1), DOWN)]
                }
                _ => unreachable!(),
            },
            '-' => match dir {
                RIGHT => vec![((x + 1, y), RIGHT)],
                LEFT => vec![((x - 1, y), LEFT)],
                UP | DOWN => {
                    vec![((x - 1, y), LEFT), ((x + 1, y), RIGHT)]
                }
                _ => unreachable!(),
            },
            'X' => vec![],
            _ => unreachable!(),
        }
    }

    fn find_strongly_connected_components(&mut self) {
        let mut tarjan = Tarjan::new(self.w * self.h * 4 - 1);
        tarjan.find_components(&self.adj);

        let n = tarjan.num_components + 1;

        // println!("Tarjan num components: {}", n);
        // println!(
        //     "highest comp id: {}",
        //     tarjan.component.iter().max().unwrap()
        // );
        // println!("lowest comp id: {}", tarjan.component.iter().min().unwrap());

        let g = tarjan
            .component
            .iter()
            .enumerate()
            .into_group_map_by(|t| t.1);

        // store which
        // self.comp_positions = vec![HashSet::with_hasher(self.hasher); n];

        for (&&comp_id, els) in &g {
            // if els.len() > 1 {
            //     println!("Interesting component (id = {comp_id}):");
            //     for (k, _) in els {
            //         let ((x, y), dir) = self.dec(*k);
            //         println!(
            //             "  {x},{y},{}",
            //             match dir {
            //                 0 => "up",
            //                 1 => "right",
            //                 2 => "down",
            //                 _ => "left",
            //             }
            //         );
            //     }
            // }

            self.comp_positions.insert(
                comp_id,
                els.into_iter()
                    .map(|&(k, _)| self.dec(k))
                    .map(|((x, y), _)| y * self.h + x)
                    .collect::<HashSet<_, FxBuildHasher>>(),
            );
        }

        self.comp = tarjan.component;

        // build the adjacency matrix for the components
        self.comp_adj = vec![vec![]; n];
        for a in 1..(self.w * self.h * 4) {
            let ac = self.comp[a];
            for &b in &self.adj[a] {
                let bc = self.comp[b];
                if ac != bc && !self.comp_adj[ac].contains(&bc) {
                    self.comp_adj[ac].push(bc);
                }
            }
        }
    }

    fn comp_reach(&mut self, comp_id: usize, i: usize) -> HashSet<usize, FxBuildHasher> {
        // let indent = String::from_utf8(vec![' ' as u8; i * 2]).unwrap();

        if let Some(res) = self.comp_reach.get(&comp_id) {
            return res.clone();
        }

        let mut res = self.comp_adj[comp_id]
            .clone()
            .into_iter()
            .flat_map(|c| self.comp_reach(c, i + 1))
            .collect::<HashSet<_, FxBuildHasher>>();

        res.extend(&self.comp_positions[&comp_id]);

        self.comp_reach.insert(comp_id, res.clone());
        // println!(
        //     "{indent}comp_reach({comp_id}) -> {}",
        //     res.iter().sum::<usize>()
        // );

        res
    }

    fn compute_reach_cached_using_components(&mut self, start: usize) -> usize {
        self.comp_reach(self.comp[start], 0).len()

        // self.comp_reach(self.comp[start], 0)
        //     .iter()
        //     .enumerate()
        //     .filter(|&(_, c)| c > &0)
        //     .map(|(k, _)| self.dec(k))
        //     .collect()
    }

    fn compute_reach_inefficient(&self, start: usize) -> usize {
        let mut todo = vec![start];
        let mut collected = vec![];

        while let Some(k) = todo.pop() {
            if !collected.contains(&k) {
                collected.push(k);
                todo.extend(self.adj[k].clone());
            }
        }

        collected
            .into_iter()
            .map(|k| self.dec(k).0)
            .collect::<HashSet<_>>()
            .len()

        // collected
        //     .into_iter()
        //     .map(|k| self.dec(k))
        //     .collect::<HashSet<_>>()
    }
}

fn bonus(input: &str, try_to_optimize_with_components: bool) -> usize {
    let grid = get_padded_grid(input);

    let mut ctx = Context::new(grid);
    println!("Fill entries..");
    ctx.fill_entries();
    println!("Fill adj matrix..");
    ctx.fill_adj_matrix();

    // // for debugging
    // println!("Find strongly connected components..");
    // ctx.find_strongly_connected_components();
    // for k in ctx.entries.clone() {
    //     println!("FOR {:?}", ctx.dec(k));
    //     println!("  using components:");
    //     let a = ctx.compute_reach_cached_using_components(k);
    //     println!("  direct:");
    //     let b = ctx.compute_reach_inefficient(k);
    //     if a != b {
    //         println!("NOT THE SAME FOR {:?}", ctx.dec(k));
    //         println!("{a:?}");
    //         println!("VS");
    //         println!("{b:?}");
    //         println!("DIFF:\n{:?}", a.symmetric_difference(&b));
    //         return 0;
    //     }
    // }

    // return 0;

    if try_to_optimize_with_components {
        println!("Find strongly connected components..");
        ctx.find_strongly_connected_components();

        println!("Compute reach...");
        // ctx.compute_reach_cached_using_components(ctx.enc(1, 1, RIGHT))
        ctx.entries
            .clone()
            .into_iter()
            .map(|start| ctx.compute_reach_cached_using_components(start))
            .max()
            .unwrap()
    } else {
        println!("Compute reach...");
        ctx.entries
            .clone()
            .into_iter()
            .map(|start| ctx.compute_reach_inefficient(start))
            .max()
            .unwrap()
    }
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
            .trim(),
            true
        ),
        51
    );

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
            .trim(),
            false
        ),
        51
    );
}
