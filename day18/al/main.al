

let example_input = "
R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
"

let up = (0, -1)
let right = (1, 0)
let down = (0, 1)
let left = (-1, 0)

let dirs = @{
  "0" right,
  "1" down,
  "2" left,
  "3" up,
}

fn cross((a, b), (c, d)) {
  a * d - c * b
}

fn area(poly) {
  let area = 0

  for let i in range(1, len(poly)-1) {
    let a = (poly[i  ][0] - poly[0][0], poly[i  ][1] - poly[0][1])
    let b = (poly[i+1][0] - poly[0][0], poly[i+1][1] - poly[0][1])
    area += cross(a, b)
  }

  (area / 2):abs
}

fn bonus(input: str) {
  let instructions = input :trim :lines :map |line| {
    let [_, (steps, _), (dir, _)] = line :matches /#([0-9a-z]{5}+)([0-9a-z])/
    (
      dirs[dir],
      steps :int 16,
    )
  }

  instructions []= instructions[0]

  let edge_corners = []
  let prev_dir = nil
  let (x, y) = (0, 0)
  for let (dir, steps) in instructions {
    let (dx, dy) = dir

    x += dx * steps
    y += dy * steps
    edge_corners []= (x, y):clone // hmm..

    prev_dir = dir
  }

  let edge_len = 0
  for let i in range(0, len(edge_corners) - 1) {
    edge_len +=
      abs(edge_corners[i+1][0] - edge_corners[i][0])
      + abs(edge_corners[i+1][1] - edge_corners[i][1])
  }

  area(edge_corners) + edge_len/2 + 1
}

print("Example bonus: {bonus(example_input)}")

// ±20ms
// 90111113594927
print("Bonus: {bonus(stdin)}")
