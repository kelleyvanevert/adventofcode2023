
fn lines(text) {
  text :split "\n"
}

fn my_max(arr) {
  let m = 0

  arr.map |el| {
    if (m < el) {
      m = el
    }
  }

  m
}

let result = stdin :split "\n\n" :map |group| { group.lines.map(int).sum } .my_max

result
