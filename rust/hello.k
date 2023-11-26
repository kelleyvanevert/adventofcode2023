
let f = |name, greeter| {
  greeter(name)
}

f("Noa") |name| {
  "Hello {name}"
}
