
let f = |name, greeter| {
  print(greeter)
  greeter(name)
}

f("Noa") |name| {
  print("Hello {name}")
}

print("hello")

run {
  42
}
