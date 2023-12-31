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

This year's challenges will (hopefully all) be completed in a new programming language _Adventlang_! [This exciting new interpreted language](#adventlang-overview) has syntax that looks like Rust, but anonymous functions like in Kotlin, postfix/infix notation function calls to make it all look very succinct, and soon-to-be-implemented GC :)

If you use VS Code, be sure to install the ✨[_official language features extension_](https://marketplace.visualstudio.com/items?itemName=hello-kelley.adventlang)✨. It currently just attempts to add some syntax highlighting, but I have high ambitions! Maybe I can get Rust code, compiled to WASM, to power a LSP? 🫣

<br clear="left" />

## Execution time stats

| Day    | AL                       | Rust                                  |
| ------ | ------------------------ | ------------------------------------- |
| Day 1  | ±2s                      | ±2.5ms                                |
| Day 2  | ±20ms                    |                                       |
| Day 3  | ±350ms                   |                                       |
| Day 4  | ±50ms                    |                                       |
| Day 5  | ±50ms                    |                                       |
| Day 6  | ±.5ms                    |                                       |
| Day 7  | ±200ms                   |                                       |
| Day 8  | ±3s                      |                                       |
| Day 9  | ±200ms                   |                                       |
| Day 10 | ±2.5s                    |                                       |
| Day 11 | ±6s                      |                                       |
| Day 12 | ±3s                      |                                       |
| Day 13 | ±100ms                   |                                       |
| Day 14 | ±20s                     | ±30ms                                 |
| Day 15 | ±130ms                   |                                       |
| Day 16 | ±600ms (only first part) | ±350ms                                |
| Day 17 |                          | ±300ms first part, ±4s bonus          |
| Day 18 | ±20ms (only bonus part)  | ±20ms                                 |
| Day 19 |                          | ±3ms                                  |
| Day 20 |                          | ±15ms                                 |
| Day 21 |                          | ±10ms                                 |
| Day 22 |                          | ±350ms                                |
| Day 23 |                          | ±300ms first part, ±1.6s bonus        |
| Day 24 |                          | <1ms first part, ±2.3s bonus using Z3 |
| Day 25 |                          | ±7s                                   |

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

  And now the time is back down to ±2.5 seconds again, and running _all_ the days in sequence went down from ±20s to ±6s! 🎉

## Day 12

Woww... today was hard! :P

First off, it look a _lot_ of Kelley-time to get the algorithm working to solve part 1. (Solving it in AL was not a problem though, it was just super finicky.)

But then, the bonus ... usually there's some kind of mathematical or programmatic trick that allows you to also solve the bonus with "a normal runtime", if only you figure it out. But .. I had a hard time figuring that out.

Finally, the only thing I could come up with is to just throw parallelism and memoizing at it, in Rust, and that apparently worked. Got it down to 15 seconds.

I also tried the memoizing in AL, but, that would have to rely on some smart dictionary implementation, which I .. don't currently have, so .. that doesn't actually fly at the moment.

## Day 13

What a refreshingly fun and simple challenge today was! So easy and lightweight in terms of performance, in comparison with yesterday :P The algorithm was cute, and the bonus generalization also, and I can compute both in AL together in ±160ms. The only thing I needed to add to AL was `continue 'label` statements, which I had prepared for already anyway. Doei!

## Day 14

Today was fun :) I'm quite happy with my little tilting algorithm (which only needs a single pass per column/row, instead of a nested loop per movable rock). And the bonus trick was a classic cycle-detection, which I was able to write in AL (it runs in just under a minute).

## Day 15

Today was messy! The challenge was not hard at all, but, somehow, for the bonus part, I did need to add a bunch of things to AL to make it look good, and did that in surpisingly messy ways .. :|

Oh well, now AL has:

- `nil` coalescing in a bunch of places, like: `list[x]?[y]?:int`
- the `if let` pattern DOES match on falsey values that ARE NOT `nil`
- a `find_index` built-in (that only really works well with the fix above)
- an `ascii` built-in that just gets the "code point" of single character strings (e.g. `c as usize` in Rust)
- `lhs ??= rhs` as a shorthand for `lhs = lhs ?? rhs`
- a bugfix where `list[i] = v` for an `i` out of bounds would accidentally fill all newly extended slots with `v` (which was due to my weird "heap implementation" btw :P)

## Day 16 and 17

It's official, we've gotten to the part of advent of code where the challenges are getting hard enough that:

1. I'm not always able to solve it, or solve it nicely, within a day. Funnily enough, the same thing happened last year on the exact same day, day 16!
2. I'm not going to be able (or willing) to solve the challenges in my own language anymore .. 😅 After yesterday (day 16)'s drama, I immediately reached for Rust today, because I just didn't have the confidence that I was going to be able to write it fully in AL, without switching to Rust again to get the performance of ergonomic flexibility that I might need.

Day 16, for me, was all about finding out (way later than I should have), that writing a memoizing recursive solution for the bonus part was not just going to fly easily, because the graph contains cycles. I spent a long time just trying to figure out how to do the memoization, until I just gave up, switch to Rust, precomputed the graph in a separate step, and then collected the counts for all the entry points the dumb way, which is Rust is .. fast enough (±5.5s). Then I spent another few hours, the next day, trying to first reduce the graph into a DAG using Tarjan's algorithm, and then computing the results recursively using memoization. But: (1) it was actually about the same speed, (2) figuring that that might just be because of the expensive HashSets I used, I tried removing them, but got stuck again.. so I just decided to move on again.

Day 17 was the first "best effort" search with a priority queue etc. that I used this year. Wasn't an easy setup either XD I guess that's what happens when you don't think about this kind of algorithms for a year, and then try to dive back in as if you still remember all the tricks and gotcha's...

Both very compelling challenges again of course! Kept my mind very entertained! :P

And the "lava fall" on the website is amazing!!

**UPDATE on day 16**

It turns out .. that if I just use `FxHashSet`, `FxHashMap`, the memoizing solution does actually greatly improve the speed, which is now down to ±1.1s 🎉

And then, using `rayon` I can get it down to ±400ms 🚀 Of course now I need to use an `Arc<Mutex<..>>`. (Not actually sure if that's the best way, but, it works for me.)

## Day 18

Today was honestly just a bit embarrasing 😅 For the first part, I just went ahead and implemented the same flood fill as I had done on day 10. For the bonus part though, I:

1. immediately noticed that that would not work, then
2. had a quick thought flash though my mind that I should just _Google the algorithm for calculating the surface of a complex polygon_, but then
3. decided that that's just boring and I should be able to, and will have more fun, coming up with an algorithm myself, so I
4. spent _HOURS_ working on it,
5. eventually just quit my approach and started on a different algorithm, which I also spent quite some time finnicking about with,
6. solved it, finally (runtime ±600ms),
7. and then just decided, _why the heck didn't I just compute the surface with a known formula_, so I did that as well — which took _me_ about 30 minutes, and subsequently took the computer .. less than 0.1 ms.

"Why" 😅

AL TODOs

- `poly:len - 1` is parsed incorrectly
- need to clone integers?!
- need to clone? `edge_corners []= (x, y):clone`

## Subsequent days

...I got too busy with Christmas-y things, to keep this README nicely updated, or to even have time every day to do the challenge :P The mini summary though:

- I had a lot of fun, albeit sometimes a bit delayed
- Nothing in AL anymore (Maybe if I compile it next year it would work, but, no)
- The bonus parts of days 20, 21 and 24 were too hard for me to figure out myself, and I needed to check Reddit/YouTube. I feel I should've been able to solve
  - Day 20's bonus was mostly a lesson in that some AoC challenges are just specificically engineered in a way that is more than just told in the description. I'll be on the lookout for this next year!
  - Day 21's bonus as well, but .. I feel I should've been able to solve it, if I used my wits a bit more, instead of trying to solve it "generally and beautifully" or whatever
  - Day 24's bonus was a big challenge for me, I immediately saw I should so something with a solver, but then I spent some full hours trying to make it work in a linear equation solver, whereas a quick check on Reddit told me I should head over to Z3 instead (a theorem prover also wrapping a bunch of more generalized solvers) because it's not even linear (which I knew, but .. I figured the linsolvers might be able to solve it anyway..), so that was just an unfortunate wrong ally I took..

A fun month of coding, all in all! 🎉

## Adventlang overview

- Mostly value-based, structural equality
- Postfix and infix function notation: `left :func right`
- Dynamically typed, functions are resolved at runtime based on types, best match wins
- Untagged union types, like in TypeScript (e.g. `?int` is just shorthand for `nil | int`)
- Garbage collected
- Syntax that looks mostly like Rust, but without the language complexities of course
- Trailing inline function arguments don't require parentheses, like in Kotlin. But also, with infix function notation, they're not necessary anyway

### Syntax example

![](./syntax_example.png)

### Types

Primitive

```
nil
int           // i64 in Rust
double        // f64 in Rust
str
regex
```

Composite

```
[T]           // because of untagged unions, these can actually be as heterogeneous as you want
list          // any list
(A,)
(A, B), ..
tuple         // any tuple
dict          // any key => any value
```

Untagged unions

```
A | B | C ...
A?            // A | nil
```

Also, types are _optional_ and there's also the type annotation `any` for if you don't care.

### Standard library

General purpose

```
print         (data: any)
run           (block: fn) -> any
clone         <T>(data: T) -> T

==            // structural equality
```

Conversions

```
int           (data: any) -> int
```

Numeric

```
min           (list: [num?]) -> num?
min           (a: num?, b: num?) -> num?
max           (list: [num?]) -> num?
max           (a: num?, b: num?) -> num?
sqrt          (x: num) -> num
ceil          (x: num) -> int
floor         (x: num) -> int
abs           (x: num) -> num
round         (x: num) -> int

+, -, *, /, <<, <, >, <=, >=
```

Boolean

```
&&, ||
```

Text

```
split         (text: str, sep: str) -> [str]
split         (text: str, sep: regex) -> [str]
lines         (text: str) -> [str]
match         (text: str, find: regex) -> (str, int)?
match_all     (text: str, find: regex) -> [(str, int)]
starts_with   (text: str, find: str) -> bool
replace       (text: str, def: (regex | str, str)) -> str
slice         (text: str, i: int) -> str
slice         (text: str, range: (int, int)) -> str
index         (text: str, i: int) -> str?
len           (text: str) -> int
trim          (text: str) -> str
chars         (text: str) -> [str]
```

Lists, tuples, dicts

```
chunks        <T>(items: [T], size: int) -> [[T]]
sort_by_key   <T>(items: [T], key: (T) -> any) -> [T]
reverse       <T>(items: [T]) -> [T]
zip           <A, B>(as: [A], bs: [B]) -> [(A, B)]
fold          <T, A>(items: [T], init: A, f: (A, T) => A) -> A
map           <A, B>(items: [A], f: (A) -> B) -> [B]
flat_map      <A, B>(items: [A], f: (A) -> [B]) -> [B]
dict          (pairs: [(any, any)]) -> Dict
in            <A>(needle: A, haystack: [A]) -> bool
in            (needle: any, haystack: Tuple) -> bool
filter        <A>(items: [A], f: (A) -> bool) -> [A]
filter_map    <A, B>(items: [A], f: (A) -> B?) -> [B]
any           (items: [any]) -> bool
all           (items: [any]) -> bool
find_map      <A, B>(items: [A], f: (A) -> B?) -> B?
find          <T>(items: [T]) -> T?
range         (start: num, end: num) -> [num]
enumerate     <T>(items: [T]) -> [(int, T)]
sum           (items: [num]) -> num
slice         <T>(items: [T], i: int) -> [T]
index         <T>(items: [T], i: int) -> T?
len           (items: [any]) -> int
```
