const fs = require("fs");

const input = fs.readFileSync("../input.txt", "utf8");

time(() => {
  // ±5ms
  solve(input);
});

time(() => {
  // ±11ms
  bonus(input);
});

function bonus(input) {
  let nums = [
    ["0", "0"],
    ["1", "1"],
    ["2", "2"],
    ["3", "3"],
    ["4", "4"],
    ["5", "5"],
    ["6", "6"],
    ["7", "7"],
    ["8", "8"],
    ["9", "9"],
    ["zero", "0"],
    ["one", "1"],
    ["two", "2"],
    ["three", "3"],
    ["four", "4"],
    ["five", "5"],
    ["six", "6"],
    ["seven", "7"],
    ["eight", "8"],
    ["nine", "9"],
    ["ten", "0"],
    ["eleven", "1"],
    ["twelve", "2"],
    ["thirteen", "3"],
    ["fourteen", "4"],
    ["fifteen", "5"],
    ["sixteen", "6"],
    ["seventeen", "7"],
    ["eighteen", "8"],
    ["nineteen", "9"],
    ["twenty", "0"],
  ];

  const solution = input
    .trim()
    .split("\n")
    .map((line) => {
      const digits = Array(line.length)
        .fill(null)
        .map((_, i) => {
          return nums.find(([pattern]) =>
            line.slice(i).startsWith(pattern)
          )?.[1];
        })
        .filter(Boolean);

      return Number(digits[0] + digits.slice(-1)[0]);
    })
    .reduce((a, b) => a + b, 0);

  console.log("Bonus:", solution);
}

function solve(input) {
  const solution = input
    .trim()
    .split("\n")
    .map((line) => {
      const digits = line.split("").filter((d) => d >= "0" && d <= "9");
      return Number(digits[0] + digits.slice(-1)[0]);
    })
    .reduce((a, b) => a + b, 0);

  console.log("Sokution:", solution);
}

function time(f) {
  const t0 = performance.now();
  f();
  console.log("  took: ", performance.now() - t0, "ms");
}
