
fn viz(grid) {
  print(grid :map |row| { row :join "" } :join "\n")
}

fn pad_grid(grid) {
  let w = grid[0] :len

  []
  + [range(0, w+2) :map |_| { "X" }]
  + grid :map |row| { ["X"] + row + ["X"] }
  + [range(0, w+2) :map |_| { "X" }]
}

fn next_pos(x, y, dir) {
  if dir == "r" {
    (x+1, y)
  } else if dir == "l" {
    (x-1, y)
  } else if dir == "u" {
    (x, y-1)
  } else {
    (x, y+1)
  }
}

let example_input = r"
.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
"

fn solve(input: str) {
  let grid = pad_grid(input :trim :lines :map chars)

  let energized = grid :map |row| { row :map |_| { 0 } }

  let done = []
  let todo = [(0, 1, "r")]

  fn queue(t) {
    if !(t :in done) {
      todo []= t
    }
  }

  while let (x,y,dir) = pop(todo) {
    done []= (x,y,dir)
    if x > 0 {
      energized[y][x] = 1
    }
    let (x2, y2) = next_pos(x, y, dir)

    let c = grid[y2][x2]
    if c == "." {
      queue((x2,y2,dir))
    } else if c == "\\" {
      if dir == "r" {
        queue((x2,y2,"d"))
      } else if dir == "d" {
        queue((x2,y2,"r"))
      } else if dir == "u" {
        queue((x2,y2,"l"))
      } else if dir == "l" {
        queue((x2,y2,"u"))
      }
    } else if c == "/" {
      if dir == "r" {
        queue((x2,y2,"u"))
      } else if dir == "d" {
        queue((x2,y2,"l"))
      } else if dir == "l" {
        queue((x2,y2,"d"))
      } else if dir == "u" {
        queue((x2,y2,"r"))
      }
    } else if c == "|" {
      if dir == "u" || dir == "d" {
        queue((x2,y2,dir))
      } else {
        queue((x2,y2,"u"))
        queue((x2,y2,"d"))
      }
    } else if c == "-" {
      if dir == "l" || dir == "r" {
        queue((x2,y2,dir))
      } else {
        queue((x2,y2,"l"))
        queue((x2,y2,"r"))
      }
    }
  }

  energized :map |row| { row :sum } :sum
}

print("Example solution: {solve(example_input)}")

// ±600ms
print("Solution: {solve(stdin)}")
