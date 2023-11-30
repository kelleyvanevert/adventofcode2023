
fn lines(text) {
  text :split "\n"
}

// just a proof-of-concept, you can also just use built-in `max`
fn my_max(arr) {
  let m = 0

  arr.map |el| {
    if (m < el) {
      m = el
    }
  }

  m
}

let result = stdin.trim :split "\n\n" :map |group| { group.lines.map(int).sum } .my_max

print("Sum of largest group: {result}")
