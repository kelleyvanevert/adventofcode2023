use std::time::Instant;

fn main() {
    let input = include_str!("../../input.txt");

    time(|| {
        solve(input);
    });

    time(|| {
        bonus(input);
    });
}

fn bonus(input: &str) {
    let nums = vec![
        ("0", 0),
        ("1", 1),
        ("2", 2),
        ("3", 3),
        ("4", 4),
        ("5", 5),
        ("6", 6),
        ("7", 7),
        ("8", 8),
        ("9", 9),
        ("zero", 0),
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9),
        ("ten", 0),
        ("eleven", 1),
        ("twelve", 2),
        ("thirteen", 3),
        ("fourteen", 4),
        ("fifteen", 5),
        ("sixteen", 6),
        ("seventeen", 7),
        ("eighteen", 8),
        ("nineteen", 9),
        ("twenty", 0),
    ];

    let solution: i64 = input
        .lines()
        .map(|line| {
            let digits = (0..line.len())
                .filter_map(|i| {
                    return nums.iter().find_map(|(pattern, digit)| {
                        if line[i..].starts_with(pattern) {
                            Some(*digit)
                        } else {
                            None
                        }
                    });
                })
                .collect::<Vec<_>>();

            format!("{}{}", digits[0], digits.last().unwrap())
                .parse::<i64>()
                .unwrap()
        })
        .sum();

    // ±3ms
    println!("Bonus: {solution}");
}

fn solve(input: &str) {
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
}

fn time<F>(mut f: F)
where
    F: FnMut(),
{
    let t0 = Instant::now();
    f();
    println!("  took {:?}", t0.elapsed());
}
