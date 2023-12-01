
let digits = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]

fn is_digit(s) {
  s :in digits
}

fn lines(text) {
  text :split "\n"
}

fn not_nil(el) {
  el != nil
}

let nums = [
  ("0", "0"),
  ("1", "1"),
  ("2", "2"),
  ("3", "3"),
  ("4", "4"),
  ("5", "5"),
  ("6", "6"),
  ("7", "7"),
  ("8", "8"),
  ("9", "9"),
  ("zero", "0"),
  ("one", "1"),
  ("two", "2"),
  ("three", "3"),
  ("four", "4"),
  ("five", "5"),
  ("six", "6"),
  ("seven", "7"),
  ("eight", "8"),
  ("nine", "9"),
  ("ten", "0"),
  ("eleven", "1"),
  ("twelve", "2"),
  ("thirteen", "3"),
  ("fourteen", "4"),
  ("fifteen", "5"),
  ("sixteen", "6"),
  ("seventeen", "7"),
  ("eighteen", "8"),
  ("nineteen", "9"),
  ("twenty", "0"),
]

fn solve(input) {
  let values = input :lines :map |line| {
    let digits = line :chars :filter is_digit
    int(digits[0] + digits[-1])
  }

  values:sum
}

fn bonus(input) {
  let values = input :lines :map |line| {
    let digits = range(0, line:len)
      :filter_map |i| {
        nums :find_map |t| {
          if (line:slice(i):starts_with(t[0])) {
            t[1]
          }
        }
      }

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

// ±2.3s
print("Bonus: {bonus(stdin:trim)}")
