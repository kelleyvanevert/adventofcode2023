
let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

fn is_digit(s) {
  s :in digits
}

fn lines(text) {
  text :split "\n"
}

fn solve(text) {
  let values = text :lines :map |line| {
    let digits = line :chars :filter is_digit
    int(digits[0] + digits[-1])
  }

  values:sum
}

let test = "1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet"

print("Test: {solve(test)}")

// ±50ms
print("Solution: {solve(stdin:trim)}")
