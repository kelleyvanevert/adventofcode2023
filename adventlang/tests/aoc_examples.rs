use adventlang::{
    parse::parse_document,
    runtime::{execute, Numeric, Value},
};

fn tuple(elements: impl IntoIterator<Item = Value>) -> Value {
    Value::Tuple(elements.into_iter().collect())
}

fn int(n: i64) -> Value {
    Value::Numeric(Numeric::Int(n))
}

#[test]
fn aoc_day01() {
    let document = r#"

let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

fn is_digit(s) {
  s :in digits
}

let nums = [
  ("0", "0"),
  ("1", "1"),
  ("2", "2"),
  ("3", "3"),
  ("4", "4"),
  ("5", "5"),
  ("6", "6"),
  ("7", "7"),
  ("8", "8"),
  ("9", "9"),
  ("zero", "0"),
  ("one", "1"),
  ("two", "2"),
  ("three", "3"),
  ("four", "4"),
  ("five", "5"),
  ("six", "6"),
  ("seven", "7"),
  ("eight", "8"),
  ("nine", "9"),
  ("ten", "0"),
  ("eleven", "1"),
  ("twelve", "2"),
  ("thirteen", "3"),
  ("fourteen", "4"),
  ("fifteen", "5"),
  ("sixteen", "6"),
  ("seventeen", "7"),
  ("eighteen", "8"),
  ("nineteen", "9"),
  ("twenty", "0"),
]

fn solve(input) {
  let values = input .lines :map |line| {
    let digits = line .chars :filter is_digit
    int(digits[0] + digits[-1])
  }

  values.sum
}

fn bonus(input) {
  let values = input .lines :map |line| {
    let digits = range(0, line.len)
      :filter_map |i| {
        nums :find_map |t| {
          if line.slice(i).starts_with(t[0]) {
            t[1]
          }
        }
      }

    int(digits[0] + digits[-1])
  }

  values.sum
}

let solution = solve("1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet");

let bonus_solution = bonus("two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen");

(solution, bonus_solution)

"#;

    assert_eq!(
        execute(&parse_document(document).unwrap(), "".into()),
        Ok(tuple([int(142), int(281)]))
    );
}

#[test]
fn aoc_day02() {
    let document = r#"

let example_input = "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
";

fn solve(input, red, green, blue) {
  input
    .trim
    .lines
    :map |game| {
      let [id, sets] = game :split ": "
      let id = id :replace ("Game ", "") .int
      let invalid = sets :split "; "
        :flat_map |set| { set :split ", " }
        :map |draw| {
          let [num, color] = draw.split(" ")
          (num.int, color)
        }
        :find |(num, color)| {
          color == "red" && num > red
          || color == "green" && num > green
          || color == "blue" && num > blue
        }

      if (invalid) {
        0
      } else {
        id
      }
    }
    .sum
}

fn bonus(input) {
  input
    .trim
    .lines
    :map |game| {
      let red = 0;
      let green = 0;
      let blue = 0;

      let sets = (game :split ": ")[1]
      sets :split "; "
        :flat_map |set| { set :split ", " }
        :map |draw| {
          let [num, color] = draw.split(" ")
          (num.int, color)
        }
        :map |(num, color)| {
          if (color == "red") {
            red = red :max num
          }
          if (color == "green") {
            green = green :max num
          }
          if (color == "blue") {
            blue = blue :max num
          }
        }

      red * green * blue
    }
    .sum
}

let solution = solve(example_input, 12, 13, 14)

let bonus_solution = bonus(example_input)

(solution, bonus_solution)

"#;

    assert_eq!(
        execute(&parse_document(document).unwrap(), "".into()),
        Ok(tuple([int(8), int(2286)]))
    );
}

#[test]
fn aoc_day03() {
    let document = r#"

let example_input = "
467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
"

fn solve(input) {
  let schematic = input.trim.lines
  let total = 0

  fn should_include(y, x, l) {
    // check previous row
    if y > 0 && schematic[y - 1] :slice ((x-1) :max 0, x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    // check current row
    if schematic[y] :slice ((x-1) :max 0, x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    // check next row
    if y < schematic.len - 1 && schematic[y + 1] :slice ((x-1) :max 0, x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    false
  }

  for let (y, line) in schematic.enumerate {
    let x = 0
    while x < line.len {
      if let m = line :slice x :match /^[0-9]+/ {
        if should_include(y, x, m[0].len) {
          total = total + (m[0].int)
        }
        x += m[0].len
      } else {
        x += 1
      }
    }
  }

  total
}

fn bonus(input) {
  let schematic = input.trim.lines
  let total = 0
  let possible_gears = @{}

  fn found_adj(pos, s) {
    if let other = possible_gears[pos] {
      total += s * other
    } else {
      possible_gears[pos] = s
    }
  }

  fn possible_gear_part(y, x, s) {
    // check previous row
    if y > 0 {
      let start = (x-1) :max 0
      if let m = schematic[y - 1] :slice (start, x + s.len + 1) :match /[*]/ {
        let pos = (y-1, start+m[1])
        found_adj(pos, s.int)
      }
    }

    // check current row
    let start = (x-1) :max 0
    if let m = schematic[y] :slice (start, x + s.len + 1) :match /[*]/ {
      let pos = (y, start+m[1])
      found_adj(pos, s.int)
    }

    // check next row
    if y < schematic.len - 1 {
      let start = (x-1) :max 0
      if let m = schematic[y + 1] :slice (start, x + s.len + 1) :match /[*]/ {
        let pos = (y+1, start+m[1])
        found_adj(pos, s.int)
      }
    }
  }

  for let (y, line) in schematic.enumerate {
    let x = 0
    while x < line.len {
      if let m = line :slice x :match /^[0-9]+/ {
        possible_gear_part(y, x, m[0])
        x += m[0].len
      } else {
        x += 1
      }
    }
  }

  total
}

let solution = solve(example_input);

let bonus_solution = bonus(example_input);

(solution, bonus_solution)

"#;

    assert_eq!(
        execute(&parse_document(document).unwrap(), "".into()),
        Ok(tuple([int(4361), int(467835)]))
    );
}

#[test]
fn aoc_day04() {
    let document = r#"

let example_input = "
Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
"

fn solve(input: str) {
  input
    .trim
    .lines
    :map |line: str| {
      let [_, rest] = line :split ":"
      let [winning, yours] = rest :split "|" :map |seg| {
        seg :match_all /[0-9]+/ :map |t| { t[0].int }
      }
      let wins = yours :filter |n| { n :in winning } .len

      if (wins > 0) {
        2 ^ (wins - 1)
      } else {
        0
      }
    }
    .sum
}

fn bonus(input: str) {
  let card_wins = input.trim.lines :map |line: str| {
    let [_, rest] = line :split ":"
    let [winning, yours] = rest :split "|" :map |seg| {
      seg :match_all /[0-9]+/ :map |t| { t[0].int }
    }
    yours :filter |n| { n :in winning } .len
  }

  let copies = card_wins :map |_| { 1 }

  for let i in range(0, card_wins.len) {
    for let w in range(1, card_wins[i] + 1) {
      copies[i + w] += copies[i]
    }
  }

  copies.sum
}

let solution = solve(example_input);

let bonus_solution = bonus(example_input);

(solution, bonus_solution)

"#;

    assert_eq!(
        execute(&parse_document(document).unwrap(), "".into()),
        Ok(tuple([int(13), int(30)]))
    );
}

#[test]
fn aoc_day05() {
    let document = r#"

let example_input = "
seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
";

fn construct_mapper(input: str) {
  let rules = input.trim.lines.slice(1) :map |line| {
    line :split " " :map int
  }

  |n: int| {
    for let [dest, source, num] in rules {
      if n >= source && n < source + num {
        return dest + (n - source)
      }
    }
    return n
  }
}

fn solve(input: str) {
  let [seeds, ..rest] = input.trim :split "\n\n"
  let seeds = seeds :replace ("seeds: ", "") :split " " :map int
  let mappers = rest :map construct_mapper

  seeds
    :map |seed| {
      for let m in mappers {
        seed = m(seed)
      }
      seed
    }
    .min
}

fn construct_smart_mapper(input: str) {
  let rules = input.trim.lines.slice(1)
    :map |line| {
      line :split " " :map int
    }
    :sort_by_key |rule| {
      rule[1]
    }

  |n: int| {
    for let [dest, source, num] in rules {
      if n < source {
        return (n, source - n)
      }
      if n >= source && n < source + num {
        return (dest + (n - source), source + num - n)
      }
    }

    return (n, 999999999)
  }
}

fn bonus(input: str) {
  let [seeds, ..rest] = input.trim :split "\n\n"
  let seeds = seeds :replace ("seeds: ", "") :split " " :map int
  let mappers = rest :map construct_smart_mapper

  let loc = nil

  for let [seed, num] in seeds :chunks 2 {
    let end = seed + num
    while seed < end {
      let n = seed
      let skip = nil
      for let m in mappers {
        let t = m(n)
        n = t[0]
        skip = t[1] :min skip
      }

      loc = loc :min n
      seed += skip
    }
  }

  loc
}

let solution = solve(example_input);

let bonus_solution = bonus(example_input);

(solution, bonus_solution)

"#;

    assert_eq!(
        execute(&parse_document(document).unwrap(), "".into()),
        Ok(tuple([int(35), int(46)]))
    );
}
