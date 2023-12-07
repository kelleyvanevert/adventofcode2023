
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

fn solve(input: str) {
  input:trim:lines
    :map |line| { line :split " " }
    :sort_by_key |[hand, bid]| { score_hand(hand) }
    :enumerate
    :map |(i, [hand, bid])| { (i + 1) * bid:int }
    :sum
}

print("Example solution: {solve(example_input)}")

// ±140ms
print("Solution: {solve(stdin)}")

// print("Example bonus: {bonus(example_input)}")
// 
// // ±100µs
// print("Bonus: {bonus(stdin)}")
