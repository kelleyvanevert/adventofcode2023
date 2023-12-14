
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
      let num_empty = 0
      for let y in range(0, h) {
        if grid[y][x] == "." {
          num_empty += 1
        } else if grid[y][x] == "#" {
          num_empty = 0
        } else if grid[y][x] == "O" && num_empty > 0 {
          grid[y][x] = "."
          grid[y - num_empty][x] = "O"
        }
      }
    }
  }

  fn tilt_south() {
    for let x in range(0, w) {
      let num_empty = 0
      for let y in range(0, h) :reverse {
        if grid[y][x] == "." {
          num_empty += 1
        } else if grid[y][x] == "#" {
          num_empty = 0
        } else if grid[y][x] == "O" && num_empty > 0 {
          grid[y][x] = "."
          grid[y + num_empty][x] = "O"
        }
      }
    }
  }

  fn tilt_west() {
    for let y in range(0, h) {
      let num_empty = 0
      for let x in range(0, w) {
        if grid[y][x] == "." {
          num_empty += 1
        } else if grid[y][x] == "#" {
          num_empty = 0
        } else if grid[y][x] == "O" && num_empty > 0 {
          grid[y][x] = "."
          grid[y][x - num_empty] = "O"
        }
      }
    }
  }

  fn tilt_east() {
    for let y in range(0, h) {
      let num_empty = 0
      for let x in range(0, w) :reverse {
        if grid[y][x] == "." {
          num_empty += 1
        } else if grid[y][x] == "#" {
          num_empty = 0
        } else if grid[y][x] == "O" && num_empty > 0 {
          grid[y][x] = "."
          grid[y][x + num_empty] = "O"
        }
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
      hs []= grid :hash
      // could alse be `:clone`, or `:map |line| { line :join "" } :join "\n"`,
      //  doesn't seem to matter much at all for performance

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

print("Example solution: {solve(example_input, false)}")

// ±80ms
print("Solution: {solve(stdin, false)}")

print("Example bonus: {solve(example_input, true)}")

// ±1 min
print("Bonus: {solve(stdin, true)}")
