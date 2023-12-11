const fs = require("fs");

// const input = `
// ...#......
// .......#..
// #.........
// ..........
// ......#...
// .#........
// .........#
// ..........
// .......#..
// #...#.....
// `;

const input = fs.readFileSync(__dirname + "/../input.txt", "utf8");

time(() => {
  // ±25ms
  solve(input, 2);
});

time(() => {
  // ±20ms
  solve(input, 1000000);
});

function solve(input, expansion) {
  const grid = input
    .trim()
    .split("\n")
    .map((line) => line.split(""));

  const rowSize = grid.map((row) => {
    if (row.includes("#")) {
      return 1;
    } else {
      return expansion;
    }
  });

  const colSize = Array(grid.length)
    .fill(null)
    .map((_, i) => {
      if (grid.some((row) => row[i] === "#")) {
        return 1;
      } else {
        return expansion;
      }
    });

  const galaxies = grid.flatMap((row, y) => {
    return row.flatMap((c, x) => {
      if (c == "#") {
        return [[x, y]];
      } else {
        return [];
      }
    });
  });

  function dist([x1, y1], [x2, y2]) {
    return (
      rowSize
        .slice(Math.min(y1, y2), Math.max(y1, y2))
        .reduce((a, b) => a + b, 0) +
      colSize
        .slice(Math.min(x1, x2), Math.max(x1, x2))
        .reduce((a, b) => a + b, 0)
    );
  }

  let total = 0;
  for (let i = 0; i < galaxies.length; i++) {
    for (let j = i + 1; j < galaxies.length; j++) {
      total += dist(galaxies[i], galaxies[j]);
    }
  }

  console.log(`Solution (expansion ${expansion}): ${total}`);
}

function time(f) {
  const t0 = performance.now();
  f();
  console.log("  took: ", performance.now() - t0, "ms");
}
