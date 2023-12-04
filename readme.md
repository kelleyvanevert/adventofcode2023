# Advent of Code 2023 🎄

See https://adventofcode.com/2023

## Links

- [Previous year's repo](https://github.com/kelleyvanevert/adventofcode2022)
- Friends also participating:
  - [Markus](https://github.com/mklinik/advent-of-code2023/)
  - [Blaž](https://codeberg.org/blazp/advent_of_code_2023)
  - [Stijn](https://github.com/AuguB/Rust_Advent_Of_Code_2023)
  - [Auke](https://github.com/Fadarrizz/advent-of-code/tree/main/2023/)

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
