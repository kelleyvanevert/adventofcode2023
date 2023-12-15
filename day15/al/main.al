
fn hash_alg(text: str) {
  let val = 0
  for let c in text :chars :map ascii {
    val = ((val + c) * 17) % 256
  }
  val
}

fn solve(input: str) {
  input :trim :split "," :map hash_alg :sum
}

print("Example solution: {solve("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")}")

// ±50ms
print("Solution: {solve(stdin)}")

//print("Example bonus: {bonus(example_input)}")

// ±30ms
//print("Bonus: {bonus(stdin)}")
