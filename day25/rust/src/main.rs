use core::panic;
use std::{
    cmp::{Ordering, Reverse},
    collections::BinaryHeap,
    time::Instant,
};

use fxhash::{FxHashMap, FxHashSet};
use itertools::Itertools;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        // ±5s
        println!("First part: {}", solve(input));
    });

    // time(|| {
    //     // ±250s
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

type Graph<'a> = FxHashMap<&'a str, FxHashSet<&'a str>>;

fn check(graph: &Graph, disconnect: Vec<&(&str, &str)>) -> Option<usize> {
    let mut groups = vec![];

    let mut vertices = graph.keys().cloned().collect::<FxHashSet<_>>();

    while let Some(&start) = vertices.iter().next() {
        let mut group = FxHashSet::default();
        let mut todo = vec![start];

        while let Some(a) = todo.pop() {
            group.insert(a);
            vertices.remove(a);

            for &b in &graph[a] {
                if disconnect.contains(&&(a, b)) || disconnect.contains(&&(b, a)) {
                    continue;
                }

                if !todo.contains(&b) && !group.contains(b) {
                    todo.push(b);
                }
            }
        }

        groups.push(group);
    }

    if groups.len() < 2 {
        None
    } else if groups.len() == 2 {
        Some(groups[0].len() * groups[1].len())
    } else {
        panic!("more than two groups?!")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Path<'a>(Vec<&'a str>);

impl<'a> Path<'a> {
    fn added(&self, next: &'a str) -> Path<'a> {
        let mut new = self.0.clone();
        new.push(next);
        Path(new)
    }
}

impl<'a> Ord for Path<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        match other.0.len().cmp(&self.0.len()) {
            Ordering::Equal => self.0.cmp(&other.0),
            ordering => ordering,
        }
    }
}

impl<'a> PartialOrd for Path<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(&other))
    }
}

fn solve(input: &str) -> usize {
    let mut graph: FxHashMap<&str, FxHashSet<&str>> = FxHashMap::default();

    for line in input.trim().lines() {
        let (source, dests) = line.split_once(": ").unwrap();
        for dest in dests.split(" ") {
            graph.entry(source).or_default().insert(dest);
            graph.entry(dest).or_default().insert(source);
        }
    }

    println!("finding all shortest paths...");
    let mut shortest: FxHashMap<(&str, &str), (Path, usize)> = FxHashMap::default();

    let mut paths: BinaryHeap<Path> =
        BinaryHeap::from_iter(graph.keys().map(|&vertex| Path(vec![vertex])));

    while let Some(path) = paths.pop() {
        // println!("explore {:?}", path.0.len());

        let first = path.0[0];
        let &last = path.0.last().unwrap();
        for &next in &graph[last] {
            let dist = path.0.len();
            if dist
                < *shortest
                    .get(&(first, next))
                    .map(|(_, d)| d)
                    .unwrap_or(&usize::MAX)
            {
                // println!("   found new shortest path {first} --({dist})-- {next}");
                let path2 = path.added(next);
                shortest.insert((first, next), (path2.clone(), dist));
                paths.push(path2);
            }
        }
    }

    // let vs = graph.keys().cloned().collect::<Vec<_>>();
    // println!("DONE");
    // println!("     {}", vs.join("  "));
    // for &a in &vs {
    //     print!("{a}  ");
    //     for &b in &vs {
    //         print!("{:>3}  ", shortest[&(a, b)].1);
    //     }
    //     println!("");
    // }

    let mut shortest_paths = shortest
        .into_iter()
        .map(|(_, (path, _))| path)
        .collect::<Vec<_>>();

    // => from longest to shortest
    shortest_paths.sort();

    println!("counting traversals...");
    let mut traversals: FxHashMap<(&str, &str), usize> = FxHashMap::default();
    for Path(path) in shortest_paths {
        for i in 0..path.len() - 1 {
            let a = path[i];
            let b = path[i + 1];
            let (a, b) = (a.min(b), a.max(b));
            *traversals.entry((a, b)).or_default() += 1;
        }
    }

    let mut traversals = traversals.into_iter().collect::<Vec<_>>();
    traversals.sort_by_key(|t| Reverse(t.1));
    // for (ab, num) in traversals {
    //     println!("{ab:?} -- {num}")
    // }
    let traversals = traversals.into_iter().map(|t| t.0).collect::<Vec<_>>();

    println!("checking removals...");

    for i in 3..traversals.len() {
        for chosen in traversals[0..i].iter().combinations(3) {
            println!("try {:?}", chosen);
            if let Some(num) = check(&graph, chosen) {
                return num;
            }
        }
    }

    unreachable!()
}

#[test]
fn test() {
    let example_input = "
jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr";

    assert_eq!(solve(example_input), 54);
}
