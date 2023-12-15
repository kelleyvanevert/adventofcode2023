use adventlang::runtime::{execute_simple, Ev};

fn list(items: impl IntoIterator<Item = Ev>) -> Ev {
    Ev::List(items.into_iter().collect())
}

fn tuple(items: impl IntoIterator<Item = Ev>) -> Ev {
    Ev::Tuple(items.into_iter().collect())
}

fn int(n: i64) -> Ev {
    Ev::Int(n)
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
  let values = input :lines :map |line: str| {
    let digits = line :chars :filter is_digit
    int(digits[0] + digits[-1])
  }

  values :sum
}

fn bonus(input: str) {
  let values = input :lines :map |line| {
    let digits = range(0, line :len)
      :filter_map |i| {
        nums :find_map |t| {
          if line :slice i :starts_with t[0] {
            t[1]
          }
        }
      }

    int(digits[0] + digits[-1])
  }

  values :sum
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

    assert_eq!(execute_simple(document), Ok(tuple([int(142), int(281)])));
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
"

fn solve(input, red, green, blue) {
  input
    :trim
    :lines
    :map |game| {
      let [id, sets] = game :split ": "
      let id = id :replace ("Game ", "") :int
      let invalid = sets :split "; "
        :flat_map |set| { set :split ", " }
        :map |draw| {
          let [num, color] = draw :split " "
          (num:int, color)
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
    :sum
}

fn bonus(input) {
  input
    :trim
    :lines
    :map |game| {
      let red = 0;
      let green = 0;
      let blue = 0;

      let sets = (game :split ": ")[1]
      sets :split "; "
        :flat_map |set| { set :split ", " }
        :map |draw| {
          let [num, color] = draw :split " "
          (num:int, color)
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
    :sum
}

let solution = solve(example_input, 12, 13, 14)

let bonus_solution = bonus(example_input)

(solution, bonus_solution)

"#;

    assert_eq!(execute_simple(document), Ok(tuple([int(8), int(2286)])));
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
  let schematic = input :trim :lines
  let total = 0

  fn should_include(y, x, l) {
    // check previous row
    if y > 0 && schematic[y - 1] :slice (max(x-1, 0), x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    // check current row
    if schematic[y] :slice (max(x-1, 0), x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    // check next row
    if y < schematic:len - 1 && schematic[y + 1] :slice (max(x-1, 0), x+l+1) :match /[-!@^&*#+%$=\/]/ {
      return true
    }

    false
  }

  for let (y, line) in schematic:enumerate {
    let x = 0
    while x < line:len {
      if let m = line :slice x :match /^[0-9]+/ {
        if should_include(y, x, m[0]:len) {
          total = total + int(m[0])
        }
        x += m[0]:len
      } else {
        x += 1
      }
    }
  }

  total
}

fn bonus(input) {
  let schematic = input :trim :lines
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
      if let m = schematic[y - 1] :slice (start, x + s:len + 1) :match /[*]/ {
        let pos = (y-1, start+m[1])
        found_adj(pos, s:int)
      }
    }

    // check current row
    let start = (x-1) :max 0
    if let m = schematic[y] :slice (start, x + s:len + 1) :match /[*]/ {
      let pos = (y, start+m[1])
      found_adj(pos, s:int)
    }

    // check next row
    if y < schematic:len - 1 {
      let start = (x-1) :max 0
      if let m = schematic[y + 1] :slice (start, x + s:len + 1) :match /[*]/ {
        let pos = (y+1, start+m[1])
        found_adj(pos, s:int)
      }
    }
  }

  for let (y, line) in schematic:enumerate {
    let x = 0
    while x < line:len {
      if let m = line :slice x :match /^[0-9]+/ {
        possible_gear_part(y, x, m[0])
        x += m[0]:len
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
        execute_simple(document),
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
    :trim
    :lines
    :map |line: str| {
      let [_, rest] = line :split ":"
      let [winning, yours] = rest :split "|" :map |seg| {
        seg :match_all /[0-9]+/ :map |t| { t[0]:int }
      }
      let wins = yours :filter |n| { n :in winning } :len

      if (wins > 0) {
        2 ^ (wins - 1)
      } else {
        0
      }
    }
    :sum
}

fn bonus(input: str) {
  let card_wins = input :trim :lines :map |line: str| {
    let [_, rest] = line :split ":"
    let [winning, yours] = rest :split "|" :map |seg| {
      seg :match_all /[0-9]+/ :map |t| { t[0]:int }
    }
    yours :filter |n| { n :in winning } :len
  }

  let copies = card_wins :map |_| { 1 }

  for let i in range(0, card_wins:len) {
    for let w in range(1, card_wins[i] + 1) {
      copies[i + w] += copies[i]
    }
  }

  copies:sum
}

let solution = solve(example_input);

let bonus_solution = bonus(example_input);

(solution, bonus_solution)

"#;

    assert_eq!(execute_simple(document), Ok(tuple([int(13), int(30)])));
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
  let rules = input :trim :lines :slice(1) :map |line| {
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
  let [seeds, ..rest] = input :trim :split "\n\n"
  let seeds = seeds :replace ("seeds: ", "") :split " " :map int
  let mappers = rest :map construct_mapper

  seeds
    :map |seed| {
      for let m in mappers {
        seed = m(seed)
      }
      seed
    }
    :min
}

fn construct_smart_mapper(input: str) {
  let rules = input :trim :lines :slice(1)
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
  let [seeds, ..rest] = input :trim :split "\n\n"
  let seeds = seeds :replace ("seeds: ", "") :split " " :map int
  let mappers = rest :map construct_smart_mapper

  let loc = nil

  for let [seed, num] in seeds :chunks 2 {
    print("seed {seed}")
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

    assert_eq!(execute_simple(document), Ok(tuple([int(35), int(46)])));
}

#[test]
fn aoc_day06() {
    let document = r#"

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

fn race((t, d)) {
    let lo = (t - sqrt((t^2) - 4 * d)) / 2
    let hi = (t + sqrt((t^2) - 4 * d)) / 2

    round(funky_floor(hi) - funky_ceil(lo) + 1)
}

fn solve(input: str) {
    let [time, dist] = input :trim :lines :map |line| {
        line :split ":" :[1] :trim :split /[ ]+/ :map int
    }

    let races = time :zip dist

    races
        :map race
        :fold 1, |a, b| { a * b }
}

fn bonus(input: str) {
    let [time, dist] = input :trim :lines :map |line| {
        line :split ":" :[1] :trim :replace (/[ ]+/, "") :int
    }

    (time, dist):race
}

let solution = solve(example_input);

let bonus_solution = bonus(example_input);

(solution, bonus_solution)

"#;

    assert_eq!(execute_simple(document), Ok(tuple([int(288), int(71503)])));
}

#[test]
fn aoc_day07() {
    let document = r#"

let example_input = "
32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
"

let five_of_a_kind = 6
let four_of_a_kind = 5
let full_house = 4
let three_of_a_kind = 3
let two_pair = 2
let one_pair = 1
let high_card = 0

fn solve(input: str) {
  let digits = @{
    "2" 2,
    "3" 3,
    "4" 4,
    "5" 5,
    "6" 6,
    "7" 7,
    "8" 8,
    "9" 9,
    "T" 10,
    "J" 11,
    "Q" 12,
    "K" 13,
    "A" 14,
  }

  fn convert_base(hand: str) {
    hand :chars :reverse :enumerate
      :map |(i, d)| { digits[d] << (i * 4) }
      :sum
  }

  fn hand_type(hand: str) {
    let counts = range(0, 16) :map |i| { (i, 0) }
    for let k in hand :chars :map |d| { digits[d] } {
      counts[k][1] += 1
    }

    if counts :find |(i, c)| { c == 5 } {
      return five_of_a_kind
    }

    if counts :find |(i, c)| { c == 4 } {
      return four_of_a_kind
    }

    if let (i, k) = counts :find |(i, c)| { c == 3 } {
      if counts :find |(j, c)| { i != j && c == 2 } {
        return full_house
      } else {
        return three_of_a_kind
      }
    }

    if let (i, k) = counts :find |(i, c)| { c == 2 } {
      if counts :find |(j, c)| { i != j && c == 2 } {
        return two_pair
      } else {
        return one_pair
      }
    }

    return high_card
  }

  fn score_hand(hand: str) {
    convert_base(hand) + (hand_type(hand) << 20)
  }

  input :trim :lines
    :map |line| { line :split " " }
    :sort_by_key |[hand, bid]| { score_hand(hand) }
    :enumerate
    :map |(i, [hand, bid])| { (i + 1) * bid:int }
    :sum
}

fn bonus(input: str) {
  let J = 0

  let digits = @{
    "J" 0, // moved (!)

    "2" 2,
    "3" 3,
    "4" 4,
    "5" 5,
    "6" 6,
    "7" 7,
    "8" 8,
    "9" 9,
    "T" 10,
    // "J" 11
    "Q" 12,
    "K" 13,
    "A" 14,
  }

  fn convert_base(hand: str) {
    hand :chars :reverse :enumerate
      :map |(i, d)| { digits[d] << (i * 4) }
      :sum
  }

  fn hand_type(hand: str) {
    let counts = range(0, 16) :map |i| { (i, 0) }
    for let k in hand :chars :map |d| { digits[d] } {
      counts[k][1] += 1
    }

    // count and then remove the jokers from the hand
    let jokers = counts[0][1]
    counts[0][1] = 0

    if counts :find |(i, c)| { c == 5 } {
      return five_of_a_kind
    }

    if counts :find |(i, c)| { c == 4 } {
      if jokers >= 1 {
        return five_of_a_kind
      } else {
        return four_of_a_kind
      }
    }

    if let (i, k) = counts :find |(i, c)| { c == 3 } {
      if jokers >= 2 {
        return five_of_a_kind
      } else if jokers >= 1 {
        return four_of_a_kind
      }

      if counts :find |(j, c)| { i != j && c == 2 } {
        return full_house
      } else {
        return three_of_a_kind
      }
    }

    if let (i, k) = counts :find |(i, c)| { c == 2 } {
      if jokers >= 3 {
        return five_of_a_kind
      } else if jokers >= 2 {
        return four_of_a_kind
      }

      if counts :find |(j, c)| { i != j && c == 2 } {
        if jokers >= 1 {
          return full_house
        } else {
          return two_pair
        }
      } else {
        if jokers >= 1 {
          return three_of_a_kind
        } else {
          return one_pair
        }
      }
    }

    if jokers >= 4 {
      return five_of_a_kind
    } else if jokers >= 3 {
      return four_of_a_kind
    } else if jokers >= 2 {
      return three_of_a_kind
    } else if jokers >= 1 {
      return one_pair
    }

    return high_card
  }

  fn score_hand(hand: str) {
    convert_base(hand) + (hand_type(hand) << 20)
  }

  input :trim :lines
    :map |line| { line :split " " }
    :sort_by_key |[hand, bid]| { score_hand(hand) }
    :enumerate
    :map |(i, [hand, bid])| { (i + 1) * bid:int }
    :sum
}

let solution = solve(example_input);

let bonus_solution = bonus(example_input);

(solution, bonus_solution)

"#;

    assert_eq!(execute_simple(document), Ok(tuple([int(6440), int(5905)])));
}

#[test]
fn aoc_day08() {
    let document = r#"

let example_input = "
RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
"

let example_input_2 = "
LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
"

fn solve(input: str) {
  let dirs = @{"L" 0, "R" 1}
  let [path, rules] = input :trim :split "\n\n"
  let path = path :chars :map |c| { dirs[c] }

  let rules = rules :lines :map |line| {
    let [source, dests] = line :split " = "
    (source, dests :replace (/[)(]/, "") :split ", ")
  } :dict

  let i = 0
  let at = "AAA"
  while at != "ZZZ" {
    at = rules[at][path[i % path:len]]
    i += 1
  }

  i
}

let bonus_example_input = "
LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
"

fn gcd(a, b) {
  if b == 0 {
    a
  } else {
    gcd(b, a % b)
  }
}

fn lcm(a, b) {
  a * b / gcd(a, b)
}

fn bonus(input: str) {
  let dirs = @{"L" 0, "R" 1}
  let [path, rules] = input :trim :split "\n\n"
  let path = path :chars :map |c| { dirs[c] }

  let nodes = rules :lines :map |line| { line :split " = " :[0] }
  let as = nodes :filter |n| { n[2] == "A" }

  let rules = rules :lines :map |line| {
    let [source, dests] = line :split " = "
    (source, dests :replace (/[)(]/, "") :split ", ")
  } :dict

  let i = 0
  let at = clone(as)
  let looping = as :map |_| { false }
  loop {
    for let j in range(0, as:len) {
      if !looping[j] {
        at[j] = rules[ at[j] ] [ path[i % path:len] ]
      }
    }

    i += 1

    for let j in range(0, as:len) {
      if !looping[j] {
        if at[j][2] == "Z" {
          print("  {as[j]} reached {at[j]} in {i} (and we happen to know it'll loop after)")
          looping[j] = i
          if all(looping) {
            print("    ..and ALL are looping now -> shortcut");
            return looping :fold 1, lcm
          }
        }
      }
    }
  }
}

let solution = solve(example_input);

let solution_2 = solve(example_input_2);

let bonus_solution = bonus(bonus_example_input);

(solution, solution_2, bonus_solution)

"#;

    assert_eq!(
        execute_simple(document),
        Ok(tuple([int(2), int(6), int(6)]))
    );
}

#[test]
fn aoc_day09() {
    let document = r#"

let example_input = "
0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
"

fn predict(values) {
  let seqs = [values]
  let i = 0
  while seqs[i] :any |n| { n != 0 } {
    seqs[i+1] = []
    for let j in range(0, seqs[i]:len - 1) {
      seqs[i+1][j] = seqs[i][j+1] - seqs[i][j]
    }
    i += 1
  }

  seqs :map |seq| { seq[-1] } :sum
}

fn solve(input: str) {
  input :trim :lines
    :map |line| { line :split " " :map int }
    :map predict
    :sum
}

fn lookback(values) {
  let seqs = [values]
  let i = 0
  while seqs[i] :any |n| { n != 0 } {
    seqs[i+1] = []
    for let j in range(0, seqs[i]:len - 1) {
      seqs[i+1][j] = seqs[i][j+1] - seqs[i][j]
    }
    i += 1
  }

  seqs :map |seq| { seq[0] } :reverse :fold 0, |m, n| { n - m }
}

fn bonus(input: str) {
  input :trim :lines
    :map |line| { line :split " " :map int }
    :map lookback
    :sum
}

let solution = solve(example_input);

let bonus_solution = bonus(example_input);

(solution, bonus_solution)

"#;

    assert_eq!(execute_simple(document), Ok(tuple([int(114), int(2)])));
}

#[test]
fn aoc_day10() {
    let document = r#"

let example_1 = "
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
"

let example_2 = "
...........
.S-------7.
.|F-----7|.
.||.....||.
.||.....||.
.|L-7.F-J|.
.|..|.|..|.
.L--J.L--J.
...........
"

let example_3 = "
.F----7F7F7F7F-7....
.|F--7||||||||FJ....
.||.FJ||||||||L7....
FJL7L7LJLJ||LJ.L-7..
L--J.L7...LJS7F-7L7.
....F-J..F7FJ|L7L7L7
....L7.F7||L7|.L7L7|
.....|FJLJ|FJ|F7|.LJ
....FJL-7.||.||||...
....L---J.LJ.LJLJ...
"

let example_4 = "
FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJ7F7FJ-
L---JF-JLJ.||-FJLJJ7
|F|F-JF---7F7-L7L|7|
|FFJF7L7F-JF7|JL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L
"

let Y = 0
let X = 1

fn is_clockwise(polygon) {
  polygon []= polygon[0]
  let sum = 0
  for let i in range(0, polygon:len - 1) {
    sum += (polygon[i+1][X] - polygon[i][X]) * (polygon[i+1][Y] + polygon[i][Y])
  }
  sum >= 0
}

fn solve(input: str) {
  print("\nSolving...");

  let grid = input :trim :lines :map |line| { line :chars }
  let start = grid :enumerate :find_map |(y, row)| {
    row :enumerate :find_map |(x, c)| {
      if c == "S" { (y, x) }
    }
  }
  let h = grid:len
  let w = grid[0]:len

  fn neighbors(p) {
    let n = []
    if p[Y] > 0   { n []= ("u", (p[Y]-1, p[X]  )) }
    if p[X] > 0   { n []= ("l", (p[Y]  , p[X]-1)) }
    if p[Y] < h-1 { n []= ("d", (p[Y]+1, p[X]  )) }
    if p[X] < w-1 { n []= ("r", (p[Y]  , p[X]+1)) }
    n
  }

  fn next((prev_dir, (y, x), _)) {
    if prev_dir == "u" && grid[y][x] == "|" { return ("u", (y-1, x  ), [ (y, x+1) ]) }
    if prev_dir == "u" && grid[y][x] == "7" { return ("l", (y  , x-1), [ (y, x+1), (y-1,x), (y-1,x+1) ]) }
    if prev_dir == "u" && grid[y][x] == "F" { return ("r", (y  , x+1), [ ]) }
    if prev_dir == "r" && grid[y][x] == "-" { return ("r", (y  , x+1), [ (y+1,x) ]) }
    if prev_dir == "r" && grid[y][x] == "7" { return ("d", (y+1, x  ), [  ]) }
    if prev_dir == "r" && grid[y][x] == "J" { return ("u", (y-1, x  ), [ (y+1,x), (y+1,x+1), (y,x+1) ]) }
    if prev_dir == "d" && grid[y][x] == "|" { return ("d", (y+1, x  ), [ (y,x-1) ]) }
    if prev_dir == "d" && grid[y][x] == "L" { return ("r", (y  , x+1), [ (y,x-1), (y+1,x-1), (y+1,x) ]) }
    if prev_dir == "d" && grid[y][x] == "J" { return ("l", (y  , x-1), [  ]) }
    if prev_dir == "l" && grid[y][x] == "-" { return ("l", (y  , x-1), [ (y-1,x) ]) }
    if prev_dir == "l" && grid[y][x] == "L" { return ("u", (y-1, x  ), [  ]) }
    if prev_dir == "l" && grid[y][x] == "F" { return ("d", (y+1, x  ), [ (y-1,x), (y-1,x-1), (y,x-1) ]) }
  }

  // (on our right-hand side, at least)
  let queue = []
  let trail = [start]

  let move = start :neighbors :find |(d, p)| { next((d, p, [])) }

  print("- tracing...")
  let i = 1
  loop {
    if grid[move[1][Y]][move[1][X]] == "S" {
      break
    } else {
      //print("  - move past {move[1]} dir ({move[0]})")
      trail []= move[1]
      move = next(move)
      for let r in move[2] {
        //print("  add to inside queue {r}")
        queue []= r
      }

      i += 1
    }
  }

  let solution = round(i / 2)
  print("  - length: {i}, solution: {solution}")

  print("- checking inside...")
  let inside = []

  let i = 0
  while i < queue:len {
    if i % 1000 == 0 {
      print("  - it {i}/{queue:len}")
    }
    let r = queue[i]
    i += 1
    //print("  - {i}, r = {r}")
    if r :in trail {
      //print(" - not inside")
    } else if r[Y] < 0 || r[X] < 0 || r[Y] >= h || r[X] >= w {
      //print(" - not in bounds")
    } else if r :in inside {
      //print(" - already done")
    } else {
      inside []= r
      //is_inside[r] = 1
      //print(" - yes  {inside:len}")

      for let (_, n) in neighbors(r) {
        if !(n :in queue) && !(n :in inside) && !(n :in trail) {
          queue []= n
        }
      }
    }
  }

  let num_inside = inside:len
  if is_clockwise(trail:clone) {
    num_inside = w * h - num_inside - trail:len
  }

  print("  - w x h: {w} x {h}, trail len: {trail:len}, clockwise: {is_clockwise(trail)}")
  print("  - inside: {num_inside}, outside: {w * h - num_inside - trail:len}")

  (solution, num_inside)
}

[
  solve(example_1),
  solve(example_2),
  solve(example_3),
  solve(example_4),
]

"#;

    assert_eq!(
        execute_simple(document),
        Ok(list([
            tuple([int(8), int(1)]),
            tuple([int(23), int(4)]),
            tuple([int(70), int(8)]),
            tuple([int(80), int(10)]),
        ]))
    );
}

#[test]
fn aoc_day11() {
    let document = r##"

let example_input = "
...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
"

fn solve(input: str, expansion: int) {
  let grid = input :trim :lines :map chars

  let row_size = grid :map |row| {
    if any(row :map |c| { c == "#" }) {
      1
    } else {
      expansion
    }
  }

  let col_size = range(0, grid[0]:len) :map |i| {
    if any(grid :map |row| { row[i] == "#" }) {
      1
    } else {
      expansion
    }
  }

  let galaxies = grid
    :enumerate
    :flat_map |(y, row)| {
      row :enumerate :flat_map |(x, c)| {
        if c == "#" {
          [(x, y)]
        } else {
          []
        }
      }
    }

  fn dist((x1, y1), (x2, y2)) {
    row_size :slice (min(y1, y2), max(y1, y2)) :sum
    + col_size :slice (min(x1, x2), max(x1, x2)) :sum
  }

  let total = 0
  for let i in range(0, galaxies:len) {
    for let j in range(i + 1, galaxies:len) {
      dist(galaxies[i], galaxies[j])
      total += dist(galaxies[i], galaxies[j])
    }
  }

  total
}

(
  solve(example_input, 2),
  solve(example_input, 10),
  solve(example_input, 100),
)

"##;

    assert_eq!(
        execute_simple(document),
        Ok(tuple([int(374), int(1030), int(8410)]))
    );
}

#[test]
fn aoc_day12() {
    let document = r##"

let example_input = "
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
"

let example_input_less = "
???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
"

fn solve(input: str) {
  let all = input :trim :lines
  let total = all:len
  all
    :enumerate
    :map |(i, line)| {
      let [records, ns] = line :split " "
      let num = arrangements(
        records :split /\.+/ :filter |b| { b },
        ns :split "," :map int,
        "  "
      )

      print("  [{i+1}/{total}] {line}   =>   {num}")
      num
    }
    :sum
}

fn hash(pieces: [str], ns: [int]) {
  (pieces :join ",") + "__" + (ns :join ",")
}

let should_memoize = false
let cache: dict[any, int] = @{}

fn arrangements(pieces, ns, indent) {
  let h = (pieces, ns)

  if should_memoize {
    if let memoized = cache[h] {
      return memoized + 0
    }
  }

  if ns:len == 0 {
    if pieces :any |p| { "#" :in p } {
      if should_memoize { cache[h] = 0 }
      return 0
    }
    if should_memoize { cache[h] = 1 }
    return 1
  }

  // find next number
  let (i, n) = ns :enumerate :fold (nil, 0), |(i, n), (j, m)| {
    if m > n { (j, m) } else { (i, n) }
  }

  //print("{indent}arrangements({pieces}) {ns}  check ({i}, {n}) -> {split_around(ns, i)}")
  let (ns_le, ns_ri) = split_around(ns, i)

  let num_possible = pieces
    :enumerate
    :filter |(j, piece)| {
      piece:len >= n
    }
    :map |(j, piece)| {
      let (le, ri) = split_around(pieces, j)
      let num = placements(piece, n)
        :map |(i, p_le, p_ri)| {
          //print("{indent}  place at {i}")
          //print("{indent}    checking left {le + p_le} {ns_le}")
          let num_le = arrangements(
            le + p_le,
            ns_le,
            indent + "      "
          )

          //print("{indent}    checking right {ri + p_ri} {ns_ri}")
          let num_ri = arrangements(
            p_ri + ri,
            ns_ri,
            indent + "      "
          )

          //print("{indent}    {num_le * num_ri}")
          num_le * num_ri
        }
        :sum
      
      //print("{indent}  {num}")
      num
    }
    :sum

  if should_memoize { cache[h] = num_possible }

  num_possible
}

fn split_around(arr, i) {
  (
    arr :slice (0, i),
    arr :slice (i + 1)
  )
}

// caching of placements improves the runtime by .. 2% :P
// what the hell, why not
let placements_cache = @{}

fn placements(piece, n) {
  if let memoized = placements_cache[(piece, n)] {
    return memoized
  }

  let s = piece:len - n + 1
  let placements = range(0, s) :filter_map |i| {
    let left_ok = i <= 0 || piece[i-1] == "?"
    let right_ok = i+n >= piece:len || piece[i+n] == "?"

    if left_ok && right_ok {
      let left = if i >= 2 {
        [piece :slice (0, i-1)]
      } else {
        []
      }

      let right = if i+n+2 <= piece:len {
        [piece :slice (i+n+1)]
      } else {
        []
      }

      (i, left, right)
    }
  }

  placements_cache[(piece, n)] = placements

  placements
}

fn bonus(input) {
  let all = input :trim :lines
  let total = all:len
  all
    :enumerate
    :map |(i, line)| {
      let [records, ns] = line :split " "
      let [records, ns] = [
        records + "?" + records + "?" + records + "?" + records + "?" + records,
        ns + "," + ns + "," + ns + "," + ns + "," + ns,
      ]
      let num = arrangements(
        records :split /\.+/ :filter |b| { b },
        ns :split "," :map int,
        "  "
      )

      print("  [{i+1}/{total}] {line}   =>   {num}")
      num
    }
    :sum
}

(
  solve(example_input),
  bonus(example_input_less),
)

"##;

    assert_eq!(execute_simple(document), Ok(tuple([int(21), int(18902)])));
}

#[test]
fn aoc_day13() {
    let document = r##"

let example_input = "
#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
"

fn detect(pattern: str, detect_smudges: int) {
  let grid = pattern :lines
  let h = grid:len
  let w = grid[0]:len

  fn find_vertical() {
    // vertical line mirroring @ x ?
    'find: for let x in range(1, w) {
      let found_smudges = 0
      // find counterexample
      // if for ANY y in (0..height), i in (0..(width-x))
      //   grid[y][x-i-1] != grid[y][x+i]
      // then NO
      for let y in range(0, h) {
        for let i in range(0, w-x) {
          if x-i-1 >= 0 && x+i < w && grid[y][x-i-1] != grid[y][x+i] {
            found_smudges += 1
            if found_smudges > detect_smudges {
              continue 'find
            }
          }
        }
      }

      //print("  found vertical line mirror around {x}")
      if found_smudges == detect_smudges {
        return x
      }
    }
  }

  fn find_horizontal() {
    // horizontal line mirroring @ y ?
    'find: for let y in range(1, h) {
      let found_smudges = 0
      // find counterexample
      // if for ANY x in (0..width), i in (0..(height-y))
      //   grid[y-i-1][x] != grid[y+i][x]
      // then NO
      for let x in range(0, w) {
        for let i in range(0, h-y) {
          if y-i-1 >= 0 && y+i < h && grid[y-i-1][x] != grid[y+i][x] {
            found_smudges += 1
            if found_smudges > detect_smudges {
              continue 'find
            }
          }
        }
      }

      //print("  found horizontal line mirror around {y}")
      if found_smudges == detect_smudges {
        return y * 100
      }
    }
  }

  find_vertical() ?? find_horizontal() ?? print("NONE FOUND") ?? 0
}

fn solve(input: str) {
  input :trim :split "\n\n" :map |pattern| { detect(pattern, 0) } :sum
}

fn bonus(input: str) {
  input :trim :split "\n\n" :map |pattern| { detect(pattern, 1) } :sum
}

(
  solve(example_input),
  bonus(example_input),
)

"##;

    assert_eq!(execute_simple(document), Ok(tuple([int(405), int(400)])));
}

#[test]
fn aoc_day14() {
    let document = r##"

let example_input = "
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

fn solve(input: str, bonus: bool) {
  let grid = input :trim :lines :map chars
  let h = grid:len
  let w = grid[0]:len

  fn tilt_north() {
    for let x in range(0, w) {
      let y = 0
      let num_empty = 0
      while y < h {
        if grid[y][x] == "." {
          num_empty += 1
        } else if grid[y][x] == "#" {
          num_empty = 0
        } else if grid[y][x] == "O" && num_empty > 0 {
          grid[y][x] = "."
          grid[y - num_empty][x] = "O"
        }
        y += 1
      }
    }
  }

  fn tilt_south() {
    for let x in range(0, w) {
      let y = h-1
      let num_empty = 0
      while y >= 0 {
        if grid[y][x] == "." {
          num_empty += 1
        } else if grid[y][x] == "#" {
          num_empty = 0
        } else if grid[y][x] == "O" && num_empty > 0 {
          grid[y][x] = "."
          grid[y + num_empty][x] = "O"
        }
        y -= 1
      }
    }
  }

  fn tilt_west() {
    for let y in range(0, h) {
      let x = 0
      let num_empty = 0
      while x < w {
        if grid[y][x] == "." {
          num_empty += 1
        } else if grid[y][x] == "#" {
          num_empty = 0
        } else if grid[y][x] == "O" && num_empty > 0 {
          grid[y][x] = "."
          grid[y][x - num_empty] = "O"
        }
        x += 1
      }
    }
  }

  fn tilt_east() {
    for let y in range(0, h) {
      let x = w-1
      let num_empty = 0
      while x >= 0 {
        if grid[y][x] == "." {
          num_empty += 1
        } else if grid[y][x] == "#" {
          num_empty = 0
        } else if grid[y][x] == "O" && num_empty > 0 {
          grid[y][x] = "."
          grid[y][x + num_empty] = "O"
        }
        x -= 1
      }
    }
  }

  if bonus {
    fn cycle() {
      tilt_north()
      tilt_west()
      tilt_south()
      tilt_east()
    }

    let hs = []
    let N = 1000000000

    let i = 0
    while i < N {
      print(" - it {i}")
      cycle()
      hs []= grid :map |line| { line :join "" } :join "\n"

      let cycle_length = range(1, i) :find |j| { hs[i] == hs[i-j] }
      if cycle_length != nil {
        let left = N-i
        let remaining = left % cycle_length - 1
        print("FOUND CYCLE! len={cycle_length}, left={left}, remaining={remaining}")
        for let k in range(0, remaining) {
          print(" - remaining it {k}")
          cycle()
        }
        break
      }

      i += 1
    }
  } else {
    tilt_north()
  }

  grid
    :reverse
    :enumerate
    :map |(y, row)| {
      (y + 1) * row :filter |c| { c == "O" } :len
    }
    :sum
}

(
  solve(example_input, false),
  solve(example_input, true),
)

"##;

    assert_eq!(execute_simple(document), Ok(tuple([int(136), int(64)])));
}
