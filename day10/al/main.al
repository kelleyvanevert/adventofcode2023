
let example_input = "
..F7.
.FJ|.
SJ.L7
|F--J
LJ...
"

fn solve(input: str) {
  let Y = 0
  let X = 1

  let grid = input :trim :lines :map |line| { line :chars }
  let start = grid :enumerate :find_map |(y, row)| {
    row :enumerate :find_map |(x, c)| {
      if c == "S" { (y, x) }
    }
  }
  let h = grid:len
  let w = grid[0]:len

  fn neighbors(p) {
    let n = []
    if p[Y] > 0   { n[] = ("u", (p[Y]-1, p[X]  )) }
    if p[X] > 0   { n[] = ("l", (p[Y]  , p[X]-1)) }
    if p[Y] < h-1 { n[] = ("d", (p[Y]+1, p[X]  )) }
    if p[X] < w-1 { n[] = ("r", (p[Y]  , p[X]+1)) }
    n
  }

  fn next((prev_dir, (y, x))) {
    if prev_dir == "u" && grid[y][x] == "|" { return ("u", (y-1, x  )) }
    if prev_dir == "u" && grid[y][x] == "7" { return ("l", (y  , x-1)) }
    if prev_dir == "u" && grid[y][x] == "F" { return ("r", (y  , x+1)) }
    if prev_dir == "r" && grid[y][x] == "-" { return ("r", (y  , x+1)) }
    if prev_dir == "r" && grid[y][x] == "7" { return ("d", (y+1, x  )) }
    if prev_dir == "r" && grid[y][x] == "J" { return ("u", (y-1, x  )) }
    if prev_dir == "d" && grid[y][x] == "|" { return ("d", (y+1, x  )) }
    if prev_dir == "d" && grid[y][x] == "L" { return ("r", (y  , x+1)) }
    if prev_dir == "d" && grid[y][x] == "J" { return ("l", (y  , x-1)) }
    if prev_dir == "l" && grid[y][x] == "-" { return ("l", (y  , x-1)) }
    if prev_dir == "l" && grid[y][x] == "L" { return ("u", (y-1, x  )) }
    if prev_dir == "l" && grid[y][x] == "F" { return ("d", (y+1, x  )) }
  }

  let move = start :neighbors :find next
  let i = 1
  loop {
    if grid[move[1][Y]][move[1][X]] == "S" {
      break
    } else {
      move = next(move)
      i += 1
    }
  }

  round(i / 2)
}

print("Example solution: {solve(example_input)}")

// ±22s
print("Solution: {solve(stdin)}")

//print("Example bonus: {bonus(example_input)}")

// ±350ms
//print("Bonus: {bonus(stdin)}")
