# Advent of Code 2023 🎄

See https://adventofcode.com/2023

## Links

- [Previous year's repo](https://github.com/kelleyvanevert/adventofcode2022)
- Friends also participating:
  - [Markus](https://github.com/mklinik/advent-of-code2023/)
  - [Blaž](https://codeberg.org/blazp/advent_of_code_2023)
  - [Stijn](https://github.com/AuguB/Rust_Advent_Of_Code_2023)
  - Auke

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
