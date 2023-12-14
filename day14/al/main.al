
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

  for let y in range(0, h) {
    'to_scan_row: for let x in range(0, w) {
      if grid[y][x] == "." {
        let y2 = y
        while y2 + 1 < h {
          y2 += 1
          if grid[y2][x] == "#" {
            continue 'to_scan_row
          } else if grid[y2][x] == "O" {
            grid[y][x] = "O"
            grid[y2][x] = "."
            continue 'to_scan_row
          }
        }
      }
    }
  }

  grid
    :reverse
    :enumerate
    :map |(y, row)| {
      (y + 1) * row :filter |c| { c == "O" } :len
    }
    :sum
}

print("Example solution: {solve(example_input)}")

// ±140ms
print("Solution: {solve(stdin)}")

//print("Example bonus: {bonus(example_input)}")

// ±140ms
//print("Bonus: {bonus(stdin)}")
