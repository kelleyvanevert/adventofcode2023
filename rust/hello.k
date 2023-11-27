
let f = |name, greeter| {
  print(greeter)
  greeter(name)
}

f("Noa") |name| {
  print("Hello {name}")
}

print("hello")

let i = 0

while (i < 20) {
  i = i + 1
  print("at {i}")
  if (i == 14) {
    return "early return! {i < 12}"
  }
}

run {
  42
}
