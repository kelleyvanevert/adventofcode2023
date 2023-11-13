import {
  seq,
  literal,
  ws1,
  alt,
  regex,
  ws0,
  late,
  Parser,
  delimited,
  many0,
  literals,
  terminated,
  sepBy0,
  optional,
  many1,
  ParseFailure,
  end,
  final,
  preceded,
} from "./lib/parser_combinators.ts";

type Identifier = {
  type: "Identifier";
  name: string;
};

const parseIdentifier = regex(/^[_a-zA-Z][_a-zA-Z0-9]*/).map(
  (name): Identifier => {
    return {
      type: "Identifier",
      name,
    };
  }
);

type StrLiteralFragment = {
  type: "StrLiteralFragment";
  str: string;
};

type StrLiteralInterpolation = {
  type: "StrLiteralInterpolation";
  expr: Expr;
};

type StrLiteralPiece = StrLiteralFragment | StrLiteralInterpolation;

type StrLiteral = {
  type: "StrLiteral";
  pieces: StrLiteralPiece[];
};

const parseStrLiteralInterpolation = late(() => {
  return delimited(literal("{"), parseExpr, literal("}")).map(
    (expr): StrLiteralInterpolation => {
      return {
        type: "StrLiteralInterpolation",
        expr,
      };
    }
  );
});

const parseStrLiteralFragment = regex(/^[^"{]+/).map(
  (str): StrLiteralFragment => {
    return {
      type: "StrLiteralFragment",
      str,
    };
  }
);

// simplified for now
const parseStr = delimited(
  literal(`"`),
  many0(alt(parseStrLiteralFragment, parseStrLiteralInterpolation)),
  literal(`"`)
).map((pieces): StrLiteral => {
  return {
    type: "StrLiteral",
    pieces,
  };
});

type NumLiteral = {
  type: "NumLiteral";
  num: number;
};

// simplified for now
const parseNum = regex(/^[0-9]+/).map((str): NumLiteral => {
  return {
    type: "NumLiteral",
    num: Number(str),
  };
});

type Variable = {
  type: "Variable";
  id: Identifier;
};

type UnaryExpr = {
  type: "UnaryExpr";
  op: string;
  expr: Expr;
};

type BinaryExpr = {
  type: "BinaryExpr";
  op: string;
  left: Expr;
  right: Expr;
};

type Expr =
  | StrLiteral
  | NumLiteral
  | Variable
  | UnaryExpr
  | BinaryExpr
  | ExprCall
  | AnonymousFn;

const parseParametersList = sepBy0(
  seq(ws0, literal(","), ws0),
  parseIdentifier,
  true
);

const parseBlock = sepBy0(
  regex(/^[ \t]*([;\n][ \t]*)+/),
  late(() => parseStmt),
  true
);

type AnonymousFn = {
  type: "AnonymousFn";
  params: Identifier[];
  block: Stmt[];
};

const parseAnonymousFn = seq(
  optional(
    delimited(literal("|"), parseParametersList, seq(ws0, literal("|"), ws0))
  ),
  delimited(seq(literal("{"), ws0), parseBlock, seq(ws0, literal("}")))
).map(([params = [], block]): AnonymousFn => {
  return {
    type: "AnonymousFn",
    params,
    block,
  };
});

const parseParenthesizedExpr = delimited(
  seq(literal("("), ws0),
  late(() => parseExpr),
  seq(ws0, literal(")"))
);

const parseExprLeaf: Parser<Expr> = alt(
  parseIdentifier.map((id): Variable => {
    return {
      type: "Variable",
      id,
    };
  }),
  parseStr,
  parseNum,
  parseParenthesizedExpr,
  parseAnonymousFn
);

type Argument = {
  type: "Argument";
  name?: Identifier;
  expr: Expr;
};

const parseArgument: Parser<Argument> = seq(
  optional(terminated(parseIdentifier, seq(ws0, literal("="), ws0))),
  late(() => parseExpr)
).map(([name, expr]) => {
  return {
    type: "Argument",
    name,
    expr,
  };
});

const parseArgumentsList = sepBy0(
  seq(ws0, literal(","), ws0),
  parseArgument,
  true
);

type ExprCall = {
  type: "ExprCall";
  expr: Expr;
  args: Argument[];
};

const parseInvocationArgs = seq(
  optional(
    delimited(
      seq(ws0, literal("("), ws0),
      parseArgumentsList,
      seq(ws0, literal(")"))
    )
  ),
  optional(
    preceded(
      ws0,
      parseAnonymousFn.map((fn): Argument => {
        return {
          type: "Argument",
          name: undefined,
          expr: fn,
        };
      })
    )
  )
).map(([args, trailingAnonymousFn]) => {
  if (!args && !trailingAnonymousFn) {
    throw new ParseFailure();
  }

  args = args || [];
  if (trailingAnonymousFn) {
    args.push(trailingAnonymousFn);
  }

  return args;
});

const parseExprCallStack: Parser<Expr> = seq(
  parseExprLeaf,
  many0(parseInvocationArgs)
).map(([expr, calls]) => {
  return calls.reduce((expr, args) => {
    return {
      type: "ExprCall",
      expr,
      args,
    };
  }, expr);
});

const parseUnaryExprStack = seq(
  many0(terminated(literal("!"), ws0)),
  parseExprCallStack
).map(([ops, expr]) => {
  return ops.reduce<Expr>((expr, op) => {
    return {
      type: "UnaryExpr",
      op,
      expr,
    };
  }, expr);
});

const parseMulExprStack = seq(
  parseUnaryExprStack,
  many0(seq(delimited(ws0, literals(["*", "/"]), ws0), parseUnaryExprStack))
).map(([expr, ops]) => {
  return ops.reduce<Expr>((left, [op, right]) => {
    return {
      type: "BinaryExpr",
      op,
      left,
      right,
    };
  }, expr);
});

const parseAddExprStack = seq(
  parseMulExprStack,
  many0(seq(delimited(ws0, literals(["+", "-"]), ws0), parseMulExprStack))
).map(([expr, ops]) => {
  return ops.reduce<Expr>((left, [op, right]) => {
    return {
      type: "BinaryExpr",
      op,
      left,
      right,
    };
  }, expr);
});

const parseExpr = parseAddExprStack;

type AssignStmt = {
  type: "AssignStmt";
  id: Identifier;
  expr: Expr;
};

type ExprStmt = {
  type: "ExprStmt";
  expr: Expr;
};

type Stmt = AssignStmt | ExprStmt;

const parseAssignStmt = seq(
  literal("let"),
  ws1,
  parseIdentifier,
  ws0,
  literal("="),
  ws0,
  parseExpr
).map((a): AssignStmt => {
  return {
    type: "AssignStmt",
    id: a[2],
    expr: a[6],
  };
});

const parseExprStmt = parseExpr.map((expr): ExprStmt => {
  return {
    type: "ExprStmt",
    expr,
  };
});

const parseStmt: Parser<Stmt> = alt(parseAssignStmt, parseExprStmt);

const parseDocument = final(delimited(ws0, parseBlock, ws0));

console.log(parseAssignStmt.parse("let  h  = !( 5 * 1- 2/4)"));

console.log(
  parseAssignStmt.parse(`let 
      
    h  = " sdf {hi} "`)
);

console.log(parseExprCallStack.parse(`f( kelley \n = 1, g() )`));

console.log(parseExprCallStack.parse(`f(|a, b ,| { 6 })`));

console.log(parseExprCallStack.parse(`f(a, { })`));

console.log(parseExprCallStack.parse(`f(a) |ctx| { let h = 8; let h = 9 }`));

console.log(
  parseDocument.parse(`

let v = "world"

View() {
  print("hello {v}")
  let h = 8

  Box |ctx| {
    Title {

    }
  }
}

`)
);
