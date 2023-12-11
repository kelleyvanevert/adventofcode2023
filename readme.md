# Advent of Code 2023 🎄

See https://adventofcode.com/2023

## Links

- [Previous year's repo](https://github.com/kelleyvanevert/adventofcode2022)
- Friends also participating:
  - [Markus](https://github.com/mklinik/advent-of-code2023/)
  - [Blaž](https://codeberg.org/blazp/advent_of_code_2023)
  - [Stijn](https://github.com/AuguB/Rust_Advent_Of_Code_2023)
  - [Auke](https://github.com/Fadarrizz/advent-of-code/tree/main/2023/)
  - [Cyril](https://github.com/c-y-r-c-l-e/adventofcode2023)

## Goals

This year I'm not really sure yet what my goals are. Last year I had a lot of fun learning how to write Rust along the way (and how to make it fast, idiomatic, etc.). But this year I was kinda planning to add something extra to the experience, in one way or another, because I was struck with how creative some people got in the AoC-community online ([this one in particular](https://github.com/HiggstonRainbird/AoC-2022)).

I did have a few ideas about how to possibly generate creative by-products, e.g. let the code also create music, or just compose something myself in Ableton .. but none of the ideas really made full sense.

Finally, in the last week or so of November, I just sat down and wrote the most basic imaginable language (parser and interpreter). I had been thinking about how to support some of my ideas the last half year with a custom language, and had been diving way too deep into the technical problems (e.g. lossless and recoverable parsing). But with AoC approaching, it just didn't make sense to get stuck on technical details, so I just wrote something quick & dirty. So maybe ... this year's AoC might revolve around extending/improving that toy language every day in order to be capable to solve that day's challenge.

Anyhow, you could summarize it as follows:

- Have fun (make beautiful or fast solutions)
- "Go with the flow" (general advice I'm trying to stick to in life 😅):
- Somehow adding something extra each day, whatever that may be:
  - Extending/improving the toy language to be capable to solving the day's challenge
  - Some other by-product or accompanying creative work

## Adventlang

<img src="./adventlang_vscode_ext/icon.png" align="left" width="200px"/>

This year's challenges will (hopefully all) be completed in a new programming language _Adventlang_! This exciting new interpreted language has syntax that looks like Rust, but anonymous functions like in Kotlin, postfix/infix notation function calls to make it all look very succinct, and soon-to-be-implemented GC :)

If you use VS Code, be sure to install the ✨[_official language features extension_](https://marketplace.visualstudio.com/items?itemName=hello-kelley.adventlang)✨. It currently just attempts to add some syntax highlighting, but I have high ambitions! Maybe I can get Rust code, compiled to WASM, to power a LSP? 🫣

<br clear="left" />

## Day 1

I extended the toy language to be able to solve today's challenge, by adding a bunch of built-in methods (`starts_with`, `slice`, `in`, `filter_map`, etc.), and adding tuples.

Notable missing features, or things I'll probably change soon:

- ditch the `UInt` type, just stick to `int/double` (or whatever I'll name them) -- it's a scripting language!
- solve the `unit/nil` issue -- I ran straight into this, now I have both, but that doesn't really make sense
- add typing features
  - I think the runtime code will be greatly improved by adding internal typing features, now I'm just manually checking and converting everywhere and it's turning into a mess
  - I'd like so have some kind of "incremental" or "optional" typing in the language design, where you can optionally type variables and parameters, and overload functions on the basis of these types. These signatures would be resolved at runtime, like in Julia
- **solve allocation-related performance issues**
  - I knew I was going to have to be smart about allocations if I wanted the toy language to be fast, but I didn't think I'd run into it on the first day already :P Solving the bonus part already takes 2.3s on average, which is incredibly high. For comparison: Rust took ±3ms, and JS took ±11ms. It's probably because of all the string slice reallocations. I know that V8 does smart copy-on-write things here, and my Rust solution .. well it explicitly doesn't copy of course :P

## Day 2

I extended AL with:

- tuple and list declaration patterns (for variable declarations and fn parameter lists)
- some operators, `&&`, `||`, `*`, `>`
- built-ins `flat_map`, `find`, `replace`
- truthiness checking in `if` and the boolean operators
- optional type annotations in declarations (and parameters)
- function signature overloading

...and all of this super minimally of course, just enough to solve today's challenges :P Which means that I left out:

- generics in type annotations (e.g. `[int]`), and pretty much any usage of these type annotations, haha
- the function signature overloading currently just checks for the number of parameters vs. arguments, but should ideally check on how well the types match. Especially when/if I add optional parameters, btw..

_Also, about the performance problems I saw yesterday:_

I actually think it's not so much the string reallocations, but mostly just all the cloning of values and fn bodies etc in the runtime. I haven't had much experience with profiling (Rust) code yet, but using [samply](https://github.com/mstange/samply/), I _think_ I can see this:

- 16% + 3.5% +2.7% —— Runtime::loopup
  - of which 6% —— Value::clone
- 7.7% + 2.3% + 3.5% —— scope values' HashMap::insert
- 10% —— FnDef::clone
- 8.5% —— scope values' HashMap::get
- 4% —— parsing

... to fix this, while satisfying Rust's borrow checker, I need to think a bit harder :P (The borrow checker will start complaining about mutably borrowing `runtime` while I'm also still referencing it, as soon as I start reworking values to be references or cows or whatever. And it's true, I probably should find a way to not have to borrow the _entire_ scope tree just to change a piece of it, or whatever. Hmm...)

Today I didn't run into any problems though, the challenge didn't involve many iterations or data ;)

## Day 3

Wow, ok, for a little fun challenge, so much functionality required :P

Here's what I added:

- `dict[key] = value` assignment —— in the most hacky way though 😅 I need to think about adding an actual _heap_ and properly treating _locations_, etc. to do it more generally
- super basic first `Dict` implementation
  - only possible literal: `@{}`, probably I'll extend this to `@{k => v, k => v}` .. kinda ugly through...
  - keys and values are both `Value`s, no generic typing so far
  - I'll have to start thinkin about equality of things now ...
- regexes and `match`
  - the literals are hacky, using an incomplete "unescaping" implementation, just like with strings btw..

And here's features that'll have to be added soon:

- some expressions should not require parentheses, I messed up orderator precedences:

  - `x + (m[0].len)`
  - `y < (schematic.len) - 1`
  - `x + (s.len) + 1`

- make the `()` in `if`, `for`, `while` loops unnecessary (or at least optional)

  - it's currently a syntactic problem because of the trailing function literal args, but .. I'm sure we can work around that, e.g. just prioritize `if` parsing over trailing function literal args

- lazy iteration if possible?

  - `enumerate` makes an iterator
  - automatic coercions, so that if it needs to be an array directly, that's OK too

- I can foresee some problems with shadowing and variable namings, for example:

  - `let len = str.len` should be fine, because usage of `.len` afterwards should select the function, not the variable, based on the fact that it better matches the desired type. But ... this "signatures" technique currently was only planned to be used for funtions, not functions AND variables. So that's a TODO, otherwise it'll get quite annoying to always have to come up with new variable names...

  - declaring a new signature for a function that already exists in a parent scope ...

- I added `if (let m = ...) { ... }`, but this should be implemented a bit more generally so that you can also do `if (y > 0 && let m = ...) { ... }`

- GC :P

## Day 4

Today was easy :) Algorithmically, as well as in that I didn't have too change too much to AL to solve it. The only things I added were:

- `arr[i] = value` assignments inside lists (which is still just implemented in a hacky ad-hoc fashion as per yesterday, but, now also for lists)
- `match_all(str, regex)` to complement `match(str, regex)`
- `^` exponentiation
- oh and I finally just added a built-in `lines(str)` as well (previous days I'd just write a helper function in the AL code to demonstrate that it can also just be implemented with `split(str, str)`)

_Afterwards, I spend some time improving AL:_

- Parentheses are no longer required (but allowed) in `if`, `while`, `for` statements. This makes it look way prettier IMO. The expressions that are accepted in these places are "constrained" in the sense that they don't allow trailing anonymous functions in function calls, at the top level. If you want to use this, just add back the parentheses.
- Fixed the indexed assignments to work more generally. Didn't actually implement it along with a heap, because that actually wasn't really necessary (yet), so, I kept it simple for now.
- Fixed the operator precedence problems of yesterday, so now you can write:

  - `x + m[0].len`
  - `y < schematic.len - 1`
  - `x + s.len + 1`

- Added `+=` etc. assignments (just as syntactic sugar)

## Day 5

Fun :)

The bonus part in particular, was a very fun little algorithmic puzzle. Last year I already noticed how in particular, Advent of Code's bonus challenges are often these delightful generalizations or subtle enhancements, and today was one like those!

Added:

- spread assignment patterns
- `<=`, `>=`
- `min` built-in
- `slice(list, i)` built-in signature

..and for the bonus part, I also needed:

- `sort_by_key`
  - for which I needed to implement `PartialOrd` for `Value`, ...and also `Ord`, although it's actually not supported though. So.. that's a bit messy, let's see how this goes..

AL TODOs

- Solve this syntax ambiguity:

  ```
  fn construct_smart_mapper(input: str) {
    let rules = input.trim.lines.slice(1)
      .map |line| {
        line :split " " :map int
      }
      :sort_by_key |rule| {
        rule[1]
      }

    // Here, the intended return closure is currently parsed as
    //  a trailing anonymous function somehow
    // (I just fixed it ad-hoc with an explicit `return` for now)
    |n: int| {
      for let [dest, source, num] in rules {
        if n < source {
          return (n, source - n)
        }
        if n >= source && n < source + num {
          return (dest + (n - source), source + num - n)
        }
      }

      return (n, 999999999)
    }
  }
  ```

## Day 6

Today was a bit mathsy! :P I guess that if you solved the first part without maths, then probably the bonus would run you into performance problems, but because I solved the maths problems in the first part already, the bonus was no additional effort.

I did entirely forget how to solve for x through, had to Google it and use some online symbolic calculator tool to realize that I needed the ABC formula and should've been able to do this myself 😅 oh well

Added to AL:

- `.[n]` syntactic sugar

  - It's nice to be able to apply indexing as if it's a unary postfix notation fn call, because then you don't have to stop midway your data flow operational notation, like in:

    ```
    line :split ":" .[1] .trim :split /[ ]+/ :map int
    ```

    of course you could already have written it like so:

    ```
    line :split ":" :index 1 .trim :split /[ ]+/ :map int
    ```

    ...but, that's kinda ugly, so, I added the `.[n]` syntax

- `zip`, `fold`
- `%`, `/`, `sqrt`, `ceil`, `floor`, `round`
- overload for built-in `str :replace (regex, str)`

Also, I apparently messed up the parser's rules for recognizing additional params to infix and postfix notation function calls. Worse actually, the syntax was not what I meant at best, or ambiguous at worst. Because, what's the parse to make of this?

```
[1, 2, 3] :fold (1) |a, b| { a * b}
```

My meaning was to be able to interpret the `(1)` as an "additional argument" (appended after the first two arguments, probably usually useful for named options, and in this case for a nice init value). But, how does the parser know it's an additional argument, and not just the right-hand argument, parenthesized? (Worse, in this case, it actually took all of `(1) |a, b| { a * b}`, i.e. an application of `1` to the anonymous fn, as the right-hand argument to `fold`. I changed this behaviour though, by now parsing the right-hand argument in _constrained_ mode. This actually also solves the syntax ambiguity problem I encountered yesterday as well.)

The _new syntax rule_, therefore, is that if you want to pass _additional arguments_ to infix or postfix notation function calls, they opening parenthesis _must directly follow_ the function identifier, whereas the right-hand argument _must_ follow at least a single space. So that now it parses like so:

```
[1, 2, 3] :fold(1) |a, b| { a * b} // fold([1,2,3], <fn>, 1)
[1, 2, 3] :fold (1) |a, b| { a * b} // fold([1,2,3], 1) .. and then some remaining syntax
```

This syntax hassle was by far the largest part of today's work :P

## Day 7

Today I spent an embarrasing amount of time fidgeting with the code 😅 I figured that in order to sort all the hands, I could just calculate them into a number, because comparing hands' cards in done sequentially (as opposed to real poker, where the highest card is used). This way, I hope(d), sorting would be super easy, because assigning a definite "score" to a hand would probably be easier than implementing comparison logic (and maybe even especially in AL).

Subsequently, I got myself into a little drama with the bonus, when I forgot to set J's score to 0, forgot to remove it from the cards counting toward a score, etc. I think I failed my answer.. 6 times or so? before getting it right. But, it works!

Added to AL:

- Dictionary literals are now complete, and the key-value pairs look like this:

  ```
  let kelley = @{
    .name "Kelley"
    .age 31
    .hobbies ["programming", "making music", "watching movies"]

    .nested @{
      .fields "ok"
    }

    "key as expr" "some value"
    this_is_a_var_btw 42
  }

  kelley.name
  kelley["name"]
  kelley.nested.fields
  people :map name // ?!
  ```

  This is a bit of a syntax design problem, because, well, `.bla` syntax has technically already been reserved for postfix notation function calls. I considered a bunch of alternatives, e.g. with other characters `@`, `#`, `$`, `->`, or simply _no dividing character_ like `person name`, and `@{ name "Kelley" }` etc. But.. they're all either just so ugly or unintuitive..

  So now I basically made implementing AL a bit harder, because a postfix `.bla` can EITHER be a dictionary item access, OR it can be a function call. This ambiguity, however, kinda also seems "cute" (?) from the perspective that properties of objects could also "mathematically" be seen as functions assigning values to their "owning" objects. Hmm.. Maybe I'm getting myself in too much trouble and will revert this decision later on..

- `<<` left bit shift
- `!=` implemented
- list built-ins: `reverse(list)`, `any(list, fn)`
- `else if` syntax (which isn't free in our case, because both branches need to be blocks, and cannot be single expressions like in some other languages)

**_UPDATE_**

I resolved the syntax problem now, by only using semicolons for both postfix and infix notation function calls. There's not much ambiguity between the two any way, either an argument follows, or it doesn't. The only reason I had the different syntax in the first place, was just because it kinda seemed cute and helpful, but, it's not such a big deal. This frees up single dot syntax for property access.

We do have one remaining syntax ambiguity left though, namely using expressions in control structure conditions, without parentheses, like so:

```
if nums :sum {
  bla
}
```

Because now the branch could just as well be an anonymous function passed to `sum`. I just resolved this following the same pattern of "constrained expressions" I had already started using. The condition expression is a constrained expression, which means it won't consume such anonymous functions, without param lists. And what if you want to use an anonymous function then? You have a few options:

- Add parentheses around the whole condition expression (parenthesized expressions are no longer constrained)

  ```
  if (nums :sum { bla }) {
    bla
  }
  ```

- Add parentheses around the anonymous function you want to use

  ```
  if nums :sum ({ bla }) {
    bla
  }
  ```

- Add a parameter list

  ```
  if nums :sum || { bla } {
    bla
  }
  ```

  ..which is very common anyway:

  ```
  if nums :find |n| { n > 0 } {
    bla
  }
  ```

A technical problem I was running into while implementing this "constrained" mode version of parsers, was that Rust's type system started faltering on figuring out the implicit return types of my parametrized parser functions, because of circularity etc.

```
fn expr_level<'a>(constrained: bool) -> impl Parser<&'a str, Output = Expr> {
  ...
}
```

After quite a while of struggling with this (and almost just ditching my own parser combinators and moving back to `nom` because I just didn't know what to do), I remembered _"the monad way"_ ✨ haha, and added the "constrainedness" mode to the "input state" of the parsers, just like `nom_locate` adds line/col info to the input state:

```
struct State<'a> {
  input: &'a str,
  constrained: bool,
}

fn expr_level(state: State) -> ParseResult<State, Expr> {
  ...
}
```

Now everything works smoothly again! :) And I can add line/col info like `nom_locate` does as well, sometime later.

## Day 8

Today was fun :)

What was kind of weird about the puzzle though (in the bonus part), is that he doesn't explicitly say that after an ghost starting at an "A" ends up at a "Z", that ghost will keep looping from that point, back to the same "Z", in the same amount of time. This does greatly simplify the code of course, but, I started out trying to figure it out more generally. I had a structure for each ghost, their "reach", and any time they'd land on a "Z" _that they had already seen before_, then from that point I'd definitely know they're looping. (And they might even be looping between a few different Z's for all I know.) The maths of it .. was going to be tricky, and indeed a bit too tricky for a day 8 challenge .. But then, it turned out, he wasn't going to use that full generality anyway :P So now the puzzle feels "a bit off", because he didn't describe it ither. Oh well, I had fun!

Added:

- `dict(pairs)` to build a dictionary from a list of (key, value) pair tuples
- `clone` -- actually implicit at the moment, but .. I'm not sure if I like that or if it will stay that way, so I figured I'd make it explicit
- `all(list, fn)` -- counterpart of `any(list, fn)`
- `break <expr>` control flow
  - and in order to add this, I finally went ahead and refactored all the return-handling in the runtime evaluation functions to be more DRY using the `Result<Value, ..>` "error" type to also include the break and return control flows: `EvaluationResult = Result<Value, EvalOther>`, where `enum EvalOther { Return, Break, RuntimeError }`
    - conceptually, I'm not 100% sure if this is super beautiful. Why count the return and break as "errors" as well? I'd say they are more like normal results than the runtime error. I think the clue is that `Result`'s `Ok` and `Err` are just slighly misnamed, because they're not _always_ the "expected result" and "unexpected result", and technically their affordances just mean _"the type of result that I want to talk about usually"_ and _"the other type of result, that .. doesn't often happen that often and I'd like to omit where possible"_. And in that case, yes, indeed, `Break` and `Return` are in the second camp, so, sure, I'll put them in `Err`.

## Day 9

Wow, ok, today's challenge was super simple and cute XD

And I didn't really have to add any new features to AL (except for commenting out a buggy type check and adding a new convenience signature for `any(list)`).

There's two things that have been on my mind though / I've been hacking on, that I still need to talk about. But I'm just noting them here for now, and will discuss them later, because today is filled with other activities :P no time

- **Type checking problems related to `any`**

  I think I underestimated the `any` type.

  - Set-theoretically, I implemented it as "all types", i.e. the `any` set it _larger_ than all other type sets. That means that in Rust's `PartialOrd` implementation for `Type`, `Type::Any >= t` for all other types `t`.
  - But, I also want to be able to pass a variable `x` of type `any` to a function that accepts only integers, like in TypeScript. But .. what? That doesn't work. Is TypeScript fooling us? I think I might've underestimated what TypeScript does to make their `any` work .. maybe it even means different types in different (syntactical) contexts.. ?
  - (Today I also ran into a version of this problem, which I recognized from earlier this week, and just commented out the relevant type-checking line before continuing..)

  To be continued..

- **Struggling to implement reference types**

  If you assign a variable holding a list to another variable, it currently copies over the list. I'm .. not super sure I want this behaviour. Anyhow, I'm _not able to not have this behaviour_ at the moment, because of the way I set up the memory of the interpreter. It doesn't have a _heap_, or maybe more precisely said, it doesn't have any way of _shared_ access/reference to memory/data, in whatever way that could be implemented. I made a few small stabs at implementing shared access (using Arcs, no heap), and yesterday spent a few hours trying to implement a heap, but I keep running into technical detail problems. Like, when implementing a heap naively, the (assignable) "locations" of values become very unwieldy and ugly. But when I try to "flatten" the whole thing into an "arena", I still have difficulties making the compiler believe everything is fine..

  To be continued..

## Day 10

**I implemented a heap and reference types.** In a very ugly direct way, but .. it was able to make day 10's bonus computable in the first place :P

Day 10 was fun and algorithmically challenging, but it was also an unpleasant surprise that, after getting the whole algorithm correct for the bonus part, AL was able to compute all the examples, but would spend ±15 minutes on the real input and then just crash. We finally got there: the moment that AL's inefficient memory usage + all the cloning of values would just not do any more.

I had been experimenting with implementing a heap, shared references, and passing values around internally as references, but it had been a struggle for a few days to get it right. Yesterday I finally made the final push, and .. now it's way faster, but also .. kinda incorrect in ways that I haven't been able to fully understand yet :P So, of all the AOC day example tests, I had to disable day 5 (because it loops), and day 7 is giving some weird results.

But the good news: day 10 now runs in ±2.5s 🤘

A bunch of refactoring and debugging lies ahead 😅

## Day 11

Fun and largely uneventful :) The new heap-based runtime still works, albeit not for the examples of day 5 and 7. And the bonus twist pointed out a fun algorithmic trick.

### Interpreter runtime optimizations

I can notice how I'm now starting to wade in the territory of optimization. This is not the kind of thing I'm naturally interested in, but now that I'm noticing big differences in speed depending on small changes in the runtime code, I'm .. starting to see why it can be striking, or even .. fun?

- The first step in this little journey was yesterday's first real performance problems: I wasn't able to run the bonus. It ran for 15 minutes and then crashed for whatever reason. I needed to stop cloning everything around, which I already knew .. and spent quite a while implementing a "heap" in the runtime. I was very happy when I finally got everything (mostly) working again, because the bonus now computed in ±2.5 seconds.

- The second step was finding out that, after resolving the remaining bugs that caused day 5 and 7 to fail, that time went back up to about 8s. The bugs were caused by accidentally overwriting other values in assignments, because now I evaluate a variable or index expression to the _location_ of a values instead of the value itself. The relevant test case:

  ```
  let a = 1; let b = a; a = 2; b // should be 1, accidentally was 2
  ```

  To fix this problem, I basically just needed a single extra line in a crucial spot: a copy of primitive values (but not composites) when I evaluate a variable expression. (Actually a few more lines though, because the same thing is also needed when evaluating index expressions like `a[2]`.)

  All this extra copying though, drove up that time that day 10 needed back from ±2.5s to ±8s.

- So now, I implemented an optimization for this: only copy _when necessary_. All the evaluations etc. in the runtime now pass around tuples `(usize, bool)` indicating not only the location of the evaluated expression (or whatever), but also whether it's a _fresh_ value or not. And if it eventually gets used for an assignment (or declaration), I use `Runtime::ensure_new` to copy it if necessary.

  And now the time is back down to ±2.5 seconds again! 🎉
