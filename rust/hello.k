
let i = 0

do {
  print("hello {i}")
  i = i + 1
  42
} while (i < 10)

fn make_counter(start) {
  let n = start
  |d| {
    if (d == 0) {
      n = 0
    } else {
      n = n + d
    }
  }
}

let g = make_counter(10)

print(g(2))
print(g(2))
print(g(0))
print(g(2))
