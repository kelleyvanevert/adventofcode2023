use std::time::Instant;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        let solution: i64 = input
            .lines()
            .map(|line| {
                let digits = line.chars().filter(|c| c.is_digit(10)).collect::<Vec<_>>();
                format!("{}{}", digits[0], digits.last().unwrap())
                    .parse::<i64>()
                    .unwrap()
            })
            .sum();

        // ±200µs
        println!("Solution: {solution}");
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
