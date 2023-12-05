
# Advent of Code 2023

I'm on the fence on which language to do this year.  I've been mostly
doing new languages, and thought of ocaml or rust, but I really like
lean.

## Day 1

I did this in Lean4 and then tried ocaml. The catch on the second part
was that the text digits could overlap, which wasn't indicated in the
example.

OCaml was painful, but so is any new language. I figured out how to import
batteries, but it still feels more verbose than other languages.

## Day 2

I got a little lost in the weeds on parsing. Just doing split felt dirty,
but Lean's parsec was, umm, lean.  And partial. So I ended up doing the split
thing, which looked fine in the end.

Later ported to OCaml. I did a little research to learn how to get the deriving
show to work, and moved the file functions to a library. Still on the fence on
whether to do the whole thing in OCaml, or just use Lean.  I'll have to pick
one once they start taking longer.

## Day 3

Not too notable.  I added `Grid` for a two dimensional array of values. I later spent part of the day adding index proofs to a copy in `Aoc2023.lean`. I'll try to adopt that on later days.

## Day 4

This one was simple and straightforward. I dealt with the `Array` indices in this one after finishing it.

## Day 5

I got in the weeds a little on part 1. I'd gone off and wrote code to compose
the maps before reading through to the end and seeing that it was much simpler
than that. Part 2 took a little more time, most just debugging the indices. I'd
like to go back and remove the `partial`.
