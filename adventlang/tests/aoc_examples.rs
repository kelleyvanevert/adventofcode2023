use adventlang::{
    parse::parse_document,
    runtime::{execute, Value},
    value::Numeric,
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
  let values = input :lines :map |line| {
    let digits = line :chars :filter is_digit
    int(digits[0] + digits[-1])
  }

  values :sum
}

fn bonus(input) {
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

    assert_eq!(
        execute(
            &parse_document(document).expect("document should parse"),
            "".into()
        ),
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

    assert_eq!(
        execute(
            &parse_document(document).expect("document should parse"),
            "".into()
        ),
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
        execute(
            &parse_document(document).expect("document should parse"),
            "".into()
        ),
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

    assert_eq!(
        execute(
            &parse_document(document).expect("document should parse"),
            "".into()
        ),
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
        execute(
            &parse_document(document).expect("document should parse"),
            "".into()
        ),
        Ok(tuple([int(35), int(46)]))
    );
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

    assert_eq!(
        execute(
            &parse_document(document).expect("document should parse"),
            "".into()
        ),
        Ok(tuple([int(288), int(71503)]))
    );
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

    assert_eq!(
        execute(
            &parse_document(document).expect("document should parse"),
            "".into()
        ),
        Ok(tuple([int(6440), int(5905)]))
    );
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
        execute(
            &parse_document(document).expect("document should parse"),
            "".into()
        ),
        Ok(tuple([int(2), int(6), int(6)]))
    );
}
