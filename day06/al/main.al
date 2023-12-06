
let example_input = "
Time:      7  15   30
Distance:  9  40  200
"

fn funky_ceil(n) {
    if abs(n % 1) < 0.0001 {
        n + 1
    } else {
        ceil(n)
    }
}

fn funky_floor(n) {
    if abs(n % 1) < 0.0001 {
        n - 1
    } else {
        floor(n)
    }
}

fn solve(input: str) {
    let [time, dist] = input:trim:lines :map |line| {
        line :split ":" :[1] :trim :split /[ ]+/ :map int
    }

    let races = time :zip dist

    races
        :map |(t, d)| {
            // print("race: t={t}, d={d}")
            let lo = (t - sqrt((t^2) - 4 * d)) / 2
            let hi = (t + sqrt((t^2) - 4 * d)) / 2
            // print("  {lo}, {hi}")
            // print("  {funky_ceil(lo)}, {funky_floor(hi)}")
            // print("  {funky_floor(hi) - funky_ceil(lo) + 1}")
            funky_floor(hi) - funky_ceil(lo) + 1
        }
        :fold 1, |a, b| { a * b }
}

print("Example solution: {solve(example_input)}")

// ±200µs
print("Solution: {solve(stdin)}")

// print("Example bonus: {bonus(example_input)}")
// 
// print("Bonus: {bonus(stdin)}")

