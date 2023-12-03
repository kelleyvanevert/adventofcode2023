
fn lines(text) {
  text :split "\n"
}

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
    if y > 0 && schematic[y - 1] :slice (max(x-1,0), x+l+1) :match /[-!@^&*#+%$=\/]/ {
      print("found")
      return true
    }

    // check current row
    if schematic[y] :slice (max(x-1,0), x+l+1) :match /[-!@^&*#+%$=\/]/ {
      print("found")
      return true
    }

    // check next row
    if y < (schematic:len) - 1 && schematic[y + 1] :slice (max(x-1,0), x+l+1) :match /[-!@^&*#+%$=\/]/ {
      print("found")
      return true
    }

    false
  }

  for let (y, line) in schematic:enumerate {
    let x = 0
    while x < line:len {
      if let m = line :slice x :match /^[0-9]+/ {
        print("match at ({x}, {y}): {m[0]}")
        if should_include(y, x, m[0]:len) {
          total = total + (m[0]:int)
        }
        x = x + (m[0]:len)
      } else {
        x = x + 1
      }
    }
  }

  total
}

print("Example: {solve(example_input)}")

print("Solution: {solve(stdin)}")
