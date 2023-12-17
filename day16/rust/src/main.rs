use std::{collections::HashSet, iter::once, time::Instant};

fn main() {
    let input = include_str!("../../input.txt");

    //time(|| {
    //    // <1ms
    //    println!("First part: {}", solve(input));
    //});

    time(|| {
        // ±5.5s
        println!("Bonus: {}", bonus(input));
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
    adj: Option<Vec<Vec<usize>>>,
    entries: Option<Vec<usize>>,
    h: usize,
    w: usize,
}

impl Context {
    fn new(grid: Grid<char>) -> Self {
        let h = grid.len();
        let w = grid[0].len();

        Self {
            grid,
            adj: None,
            entries: None,
            h,
            w,
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

        self.entries = Some(entries);
    }

    fn fill_adj_matrix(&mut self) {
        let mut todo = self.entries.clone().unwrap();
        let mut done = vec![false; self.w * self.h * 4];
        let mut adj = vec![vec![]; self.w * self.h * 4];
        while let Some(k) = todo.pop() {
            if !done[k] {
                let ((x, y), dir) = self.dec(k);
                let ns = self
                    .next(x, y, dir, self.grid[y][x])
                    .into_iter()
                    .map(|((x, y), dir)| self.enc(x, y, dir))
                    .collect::<Vec<_>>();
                adj[k] = ns.clone();
                done[k] = true;
                // println!("{:?} -> {:?}", ((x, y), dir), ns);
                todo.extend(ns);
            }
        }

        self.adj = Some(adj);
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

    fn count(&self, start: usize) -> usize {
        println!("gather {:?}...", self.dec(start));

        let mut todo = vec![start];
        let mut collected = vec![];

        while let Some(k) = todo.pop() {
            if !collected.contains(&k) {
                collected.push(k);
                todo.extend(self.adj.as_ref().unwrap()[k].clone());
            }
        }

        // found.iter().sum::<usize>()
        collected
            .into_iter()
            .map(|k| self.dec(k).0)
            .filter(|&(x, y)| self.grid[y][x] != 'X')
            .collect::<HashSet<_>>()
            .len()
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

    let mut ctx = Context::new(grid);
    ctx.fill_entries();
    ctx.fill_adj_matrix();

    let entries = ctx.entries.clone().unwrap();

    entries
        .into_iter()
        .map(|start| ctx.count(start))
        .max()
        .unwrap()
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
