
fn hash(text: str) {
  let val = 0
  for let c in text :chars :map ascii {
    val = ((val + c) * 17) % 256
  }
  val
}

fn solve(input: str) {
  input :trim :split "," :map hash :sum
}

fn bonus(input: str) {
  let boxes = []

  for let step in input :trim :split "," {
    let m = step :matches /([a-z]+)([-=])([0-9]+)?/
    let label = m[1][0]
    let i = label :hash
    let op = m[2][0]
    let lens = m[3]?[0] ?:int

    boxes[i] ??= []
    if op == "=" {
      if let j = boxes[i] :find_index |b| { b[0] == label } {
        boxes[i][j] [1] = lens
      } else {
        boxes[i] []= (label, lens)
      }
    } else {
      if let j = boxes[i] :find_index |b| { b[0] == label } {
        boxes[i] :remove_at j
      }
    }
  }

  boxes
    :enumerate
    :map |(i, box = [])| {
      box
        :enumerate
        :map |(slot, (_, focal_length))| {
          (i + 1) * (slot + 1) * focal_length
        }
        :sum
    }
    :sum
}

let example_input = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"

print("Example solution: {solve(example_input)}")

// ±50ms
print("Solution: {solve(stdin)}")

print("Example bonus: {bonus(example_input)}")

// ±90ms
print("Bonus: {bonus(stdin)}")
