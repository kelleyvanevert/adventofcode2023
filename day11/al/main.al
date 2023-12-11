
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

fn solve(input: str) {
  let input_lines = input :trim :lines

  let is_empty_col = range(0, input_lines[0]:len) :map |i| {
    input_lines :map |line| { line[i] == "." } :all
  }

  let expanded_grid = input_lines
    :flat_map |line| {
      if line :match /#/ {
        [line]
      } else {
        [line, line]
      }
    }
    :map |line| { line :chars }
    :map |line| {
      line :enumerate :flat_map |(x, c)| {
        if is_empty_col[x] {
          [c, c]
        } else {
          [c]
        }
      }
    }

  let galaxies = expanded_grid
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

  //print("galaxies at {galaxies}")

  let total = 0
  for let i in range(0, galaxies:len) {
    for let j in range(i + 1, galaxies:len) {
      let (x1, y1) = galaxies[i]
      let (x2, y2) = galaxies[j]
      total += abs(x2 - x1) + abs(y2 - y1)
    }
  }

  total
}

print("Example solution: {solve(example_input)}")

// ±600ms
print("Solution: {solve(stdin)}")

//print("Example bonus: {bonus(example_input)}")

// ±350ms
//print("Bonus: {bonus(stdin)}")
