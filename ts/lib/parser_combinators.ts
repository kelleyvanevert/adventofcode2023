export class ParseFailure extends Error {
  constructor(message = "failed to parse") {
    super(message);
  }
}

export type ParseResult<T> = [T, string];

export class Parser<T = any> {
  constructor(public parse: (input: string) => ParseResult<T>) {}

  map<U>(transform: (result: T) => U) {
    return new Parser<U>((input) => {
      const [result, remaining] = this.parse(input);
      return [transform(result), remaining];
    });
  }
}

export function literals(lits: string[]) {
  return new Parser((text) => {
    const lit = lits.find((lit) => text.startsWith(lit));
    if (typeof lit === "string") {
      return [lit, text.slice(lit.length)];
    } else {
      throw new ParseFailure();
    }
  });
}

export function literal(lit: string) {
  return literals([lit]);
}

export const end = new Parser((text) => {
  if (text.length === 0) {
    return [undefined, text];
  } else {
    throw new ParseFailure();
  }
});

export function final<T>(parser: Parser<T>) {
  return terminated(parser, end);
}

export function regex(r: RegExp) {
  return new Parser((text) => {
    const m = text.match(r);
    if (m) {
      return [m[0], text.slice(m[0].length)];
    } else {
      throw new ParseFailure();
    }
  });
}

export function optional<T>(parser: Parser<T>) {
  return new Parser((input) => {
    try {
      return parser.parse(input);
    } catch (error) {
      if (error instanceof ParseFailure) {
        return [undefined, input];
      } else {
        throw error;
      }
    }
  });
}

export const ws0 = regex(/^\s*/);
export const ws1 = regex(/^\s+/);

export function seq(): Parser<[]>;
export function seq<A>(a: Parser<A>): Parser<[A]>;
export function seq<A, B>(a: Parser<A>, b: Parser<B>): Parser<[A, B]>;
export function seq<A, B, C>(
  a: Parser<A>,
  b: Parser<B>,
  c: Parser<C>
): Parser<[A, B, C]>;
export function seq<A, B, C, D>(
  a: Parser<A>,
  b: Parser<B>,
  c: Parser<C>,
  d: Parser<D>
): Parser<[A, B, C, D]>;
export function seq<A, B, C, D, E>(
  a: Parser<A>,
  b: Parser<B>,
  c: Parser<C>,
  d: Parser<D>,
  e: Parser<E>
): Parser<[A, B, C, D, E]>;
export function seq<A, B, C, D, E, F>(
  a: Parser<A>,
  b: Parser<B>,
  c: Parser<C>,
  d: Parser<D>,
  e: Parser<E>,
  f: Parser<F>
): Parser<[A, B, C, D, E, F]>;
export function seq<A, B, C, D, E, F, G>(
  a: Parser<A>,
  b: Parser<B>,
  c: Parser<C>,
  d: Parser<D>,
  e: Parser<E>,
  f: Parser<F>,
  g: Parser<G>
): Parser<[A, B, C, D, E, F, G]>;
export function seq(...parsers: Parser<any>[]) {
  return new Parser((text) => {
    return [
      parsers.map((parser) => {
        let [result, remaining] = parser.parse(text);
        text = remaining;
        return result;
      }),
      text,
    ];
  });
}

export function preceded<T>(pre: Parser<any>, t: Parser<T>) {
  return seq(pre, t).map((a) => a[1]);
}

export function terminated<T>(t: Parser<T>, post: Parser<any>) {
  return seq(t, post).map((a) => a[0]);
}

export function delimited<T>(
  pre: Parser<any>,
  t: Parser<T>,
  post: Parser<any>
) {
  return seq(pre, t, post).map((a) => a[1]);
}

export function alt(): Parser<never>;
export function alt<A>(a: Parser<A>): Parser<A>;
export function alt<A, B>(a: Parser<A>, b: Parser<B>): Parser<A | B>;
export function alt<A, B, C>(
  a: Parser<A>,
  b: Parser<B>,
  c: Parser<C>
): Parser<A | B | C>;
export function alt<A, B, C, D>(
  a: Parser<A>,
  b: Parser<B>,
  c: Parser<C>,
  d: Parser<D>
): Parser<A | B | C | D>;
export function alt<A, B, C, D, E>(
  a: Parser<A>,
  b: Parser<B>,
  c: Parser<C>,
  d: Parser<D>,
  e: Parser<E>
): Parser<A | B | C | D | E>;
export function alt(...parsers: Parser<any>[]) {
  return new Parser((input) => {
    for (const parser of parsers) {
      try {
        return parser.parse(input);
      } catch (error) {
        if (!(error instanceof ParseFailure)) {
          throw error;
        }
      }
    }

    throw new ParseFailure();
  });
}

function many<T>(item: Parser<T>, atLeast = 0) {
  return new Parser((input) => {
    const results: T[] = [];
    while (true) {
      try {
        const [result, remaining] = item.parse(input);
        results.push(result);
        input = remaining;
      } catch (error) {
        if (error instanceof ParseFailure && results.length >= atLeast) {
          return [results, input];
        } else {
          throw error;
        }
      }
    }
  });
}

export function many0<T>(item: Parser<T>) {
  return many(item, 0);
}

export function many1<T>(item: Parser<T>) {
  return many(item, 1);
}

function sepBy<T>(
  sep: Parser<any>,
  item: Parser<T>,
  atLeast = 0,
  allowDelim = false
) {
  return new Parser((input) => {
    const results: T[] = [];
    let delimited = false;
    while (true) {
      try {
        if (results.length > 0) {
          const [, remaining] = sep.parse(input);
          input = remaining;
          delimited = true;
        }

        const [result, remaining] = item.parse(input);
        results.push(result);
        input = remaining;
        delimited = false;
      } catch (error) {
        if (error instanceof ParseFailure && results.length >= atLeast) {
          if (!allowDelim && delimited) {
            throw error;
          }
          return [results, input];
        } else {
          throw error;
        }
      }
    }
  });
}

export function sepBy0<T>(
  sep: Parser<any>,
  item: Parser<T>,
  allowDelim = false
) {
  return sepBy(sep, item, 0, allowDelim);
}

export function sepBy1<T>(
  sep: Parser<any>,
  item: Parser<T>,
  allowDelim = false
) {
  return sepBy(sep, item, 1, allowDelim);
}

export function late<T>(getParser: () => Parser<T>) {
  return new Parser((input) => {
    return getParser().parse(input);
  });
}
