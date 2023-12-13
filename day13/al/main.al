
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

fn detect(pattern: str) {
  let grid = pattern :lines
  let h = grid:len
  let w = grid[0]:len

  fn find_vertical() {
    // vertical line mirroring @ x ?
    'find: for let x in range(1, w) :reverse {
      // find counterexample
      // if for ANY y in (0..height), i in (0..(width-x))
      //   grid[y][x-i-1] != grid[y][x+i]
      // then NO
      for let y in range(0, h) {
        for let i in range(0, w-x) {
          if x-i-1 >= 0 && x+i < w && grid[y][x-i-1] != grid[y][x+i] {
            continue 'find
          }
        }
      }

      print("  found vertical line mirror around {x}")
      return x
    }
  }

  fn find_horizontal() {
    // horizontal line mirroring @ y ?
    'find: for let y in range(1, h) :reverse {
      // find counterexample
      // if for ANY x in (0..width), i in (0..(height-y))
      //   grid[y-i-1][x] != grid[y+i][x]
      // then NO
      for let x in range(0, w) {
        for let i in range(0, h-y) {
          if y-i-1 >= 0 && y+i < h && grid[y-i-1][x] != grid[y+i][x] {
            continue 'find
          }
        }
      }

      print("  found horizontal line mirror around {y}")
      return y * 100
    }
  }

  find_vertical() ?? find_horizontal() ?? 0
}

fn solve(input: str) {
  input :trim :split "\n\n" :map detect :sum
}

print("Example solution: {solve(example_input)}")

// ±70ms
print("Solution: {solve(stdin)}")

//print("Example bonus: {bonus(example_input)}")

// ?
//print("Bonus: {bonus(stdin)}")
