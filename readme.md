
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

Went back and did `day5v2.lean`, which is total by structural recursion.  I think
the `apply'` is more clear this way.  I did make a few mistakes (fixed by each occurrence of `min`) that could have been detected by a length in = total length out
constraint.

## Day 6

This was an easy day, and I got to break out the quadratic formula.

## Day 7

Fairly straightforward.  For the first pass I just copied and edited for part2, because I was working
from my phone.  I then went back and cleaned things up.

## Day 8

This one went fairly quickly.  I grabbed `lcm` from last year, and used `Parsec` this time.

## Day 9

Was really quick, but I should go back and show totality.

## Day 10

For part 2, I recalled some trick about counting line crossings for filling curves, that I think is used in PDF rendering. So I took that approach. I later went back and took out the N, S, E, W bitmasks and used the original letters instead.  It made the code a little clearer.

## Day 11

Part 2 involved changing one number, I already had the right data structure in place. (I
went back and added an argument.)

## Day 12

I did part 1 with some heuristics for cutting the search. Probably should have checked that in. Part 2 I recognised as a dynamic programming problem and added memoization. I then spent a bunch of time debugging working code, beacuse I'd missed the "separated by ?" in the instructions.  And I hit a couple of snags matching on multiple values and had to build out the case trees manually.

I went back and showed totality, had to add a theorem that drop return its arg or something smaller.

## Day 18

Day 18 was a bit of a beast. For part 1 I expanded everything out and did a fill (not checked
in), but had some off by one errors to deal with. After looking at the example, I made an assumption that the coordinates didn't go negative (i.e. I used `Nat`), which led to a bunch
of debugging before I realized my mistake. I also had a stray copy of of the grid in the set
method, causing `O(n)` updates instead of `O(1)`.

Part 2 took a while, I started by partitioning to virtual rows and columns for each
horizontal/vertical point and filling. There were a lot of edge cases, so I switched to the
Day 10 algorithm, which also had a lot of edge cases. Recasting part 1 to the part 2 code
helped debugging.

## Day 19

Also a pain. For part two I had a logic error that caused me to go back and rework things all way back to the parser and data representation (and had to wait until after work and making dinner to finish). I went back and adapted part1 to the new structure.  Probably could use some clean up, but I'm taking a break.

In part two, I had to deal with ranges across four dimensions. I just ran them forward through the instructions, taking all of the possible branches. At the end I had to sort out the union of them. I vaguely remembered something from a previous year that involved adding something, and then subtracing the intersection with the previous stuff (positive and negative), adding those intersections onto the list of previous stuff (with the sign flipped). That's kind of exponential, but a lot of them filter out as empty.

## Day 21

I almost didn't make this one. Brute force was impossible. I wanted to rule out the center, noticed it was diamond shape (with an alternating grid
over the blank spaces) and did a bunch of work to work out the diamond edges (piecing them together to whole blocks, subtracting extra diagonal lines, etc). It didn't work because there are a bunch of imperfections on the edges.  After discovering that, I ran a 5x5 block of the original
just long enough to fill to the edge and copyied each of the macro blocks out of that. 

There were still a few transcription errors (I pulled a top right instead of bottom right for outer edge), but I eventually found them and the answer popped out. I probably submitted a dozen tries over the course of the day though.
