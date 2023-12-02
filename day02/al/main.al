
fn lines(text) {
  text :split "\n"
}

let example_input = "
Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
";

fn solve(input, red, green, blue) {
  input
    :trim
    :lines
    :map |line| {
      let [id, sets] = line :split ": "
      let id = id :replace ("Game ", "") :int
      let invalid = sets :split "; "
        :flat_map |set| { set :split ", " }
        :map |draw| {
          let [num, color] = draw:split(" ")
          (num:int, color)
        }
        :find |t| {
          t[1] == "red" && t[0] > red || t[1] == "green" && t[0] > green || t[1] == "blue" && t[0] > blue
        }

      if (invalid) {
        0
      } else {
        id
      }
    }
    :sum
}

print("Example: {solve(example_input, 12, 13, 14)}")

print("Solution: {solve(stdin, 12, 13, 14)}")
