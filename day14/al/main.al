
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

fn solve(input: str) {
  let grid = input :trim :lines :map chars
  let h = grid:len
  let w = grid[0]:len

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

//  print(grid :map |line| { line :join "" } :join "\n")

  grid
    :reverse
    :enumerate
    :map |(y, row)| {
      (y + 1) * row :filter |c| { c == "O" } :len
    }
    :sum
}

print("Example solution: {solve(example_input)} should be 136")

// ±100ms
print("Solution: {solve(stdin)} should be 107951")

//print("Example bonus: {bonus(example_input)}")

// ±140ms
//print("Bonus: {bonus(stdin)}")
