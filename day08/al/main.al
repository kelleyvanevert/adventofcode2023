
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

print("Example solution: {solve(example_input)}")

// ±1.5s
print("Solution: {solve(stdin)}")

// print("Example bonus: {bonus(example_input)}")
// 
// // ±100µs
// print("Bonus: {bonus(stdin)}")

