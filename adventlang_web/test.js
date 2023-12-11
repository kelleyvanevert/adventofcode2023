const { greet, executeAdventCode } = require("./pkg/adventlang_web");

greet("Kelley");

console.log(
  executeAdventCode(
    'let a = [0, 1, nil]; a[0] = 5.1; let d = @{}; d[a] = "hi"; d'
  )
);
