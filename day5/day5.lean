import Lean
import Lean.Data.Parsec
open Lean.Parsec

-- I'll need ranges, but the maps are the same size...
-- let's parse first.

structure MapEntry where
  dest : Nat
  src  : Nat
  len  : Nat
deriving Repr

abbrev Map := List MapEntry

structure Problem where
  seeds : List Nat
  maps : List Map
deriving Repr

def nums (s : String) := (s.splitOn.filter (·.length > 0)).mapA (·.trim.toNat?)

def parseEntry (part : String) : Option MapEntry := do
  let [dest, src, len] <- nums part | dbg_trace "bad entry {part}";none
  .some ⟨ dest, src, len ⟩

def parseMap (part : List String) : Option Map := do
  let (_ :: part) := part | .none
  part.mapM parseEntry

def parseFile (content : String) : Option Problem := do
  let parts := content.trim.splitOn "\n\n"
  let (first :: rest) := parts | dbg_trace "ferret";.none
  let [_, x] := first.splitOn ": " | .none
  let .some seeds := nums x | none
  let maps <- rest.mapA (λ part => parseMap (part.splitOn "\n"))
  .some { seeds, maps }

def applyEntry (n : Nat) (e : MapEntry) :=
  if n >= e.src && n < e.src + e.len then n + e.dest - e.src else n

def applyMap (n : Nat) : Map -> Nat
| [] => n
| e :: es =>  if n >= e.src && n < e.src + e.len then n + e.dest - e.src else applyMap n es

def part1 (p : Problem) : IO Unit := do
  let loc := p.seeds.map (λ s => p.maps.foldl applyMap s)
  let part1 := loc.foldl min 999999999
  println! "part1 {part1}"

abbrev Range := Nat × Nat

-- assumes sorted
partial
def apply' (r : Range) : List MapEntry -> List Range
| [] => [r]
| x@(⟨ d,s,l ⟩ :: es) =>
     if r.1 + r.2 <= s then [r] -- map after range
     else if s + l <= r.1 then apply' r es -- map before range
     -- take off any bare range on front
     else if r.1 < s then (r.1, s - r.1) :: apply' (s, r.2 + r.1 - s) x
     -- we have a match
     else if s + l < r.1 + r.2 then
        let slack := r.1 - s
        (r.1 + d - s, l - slack) :: apply' (r.1 + l - slack, r.2 + slack - l) x
     else [(r.1 + d - s, r.2)]

def apply (ranges : List Range) (entries : List MapEntry) :=
  let entries := (entries.toArray.qsort (·.2 < ·.2)).toList
  --dbg_trace "# apply {repr entries}"
  let apply'' r me :=
    let r' := apply' r me
    --dbg_trace "##  {r} -> {r'}"
    r'
  let res := (ranges.map (apply'' · entries)).join
  --dbg_trace "{ranges} -> {res}"
  res

def mkRanges : List Nat -> Option (List Range)
| a :: b :: rs => mkRanges rs >>= ((a,b) :: ·)
| [] => .some []
| _ => .none

def part2 (p : Problem): IO Unit := do
  -- println! (repr p)
  let .some ranges := mkRanges p.seeds | println! "odd seeds"
  let results := p.maps.foldl apply ranges
  let results := results.toArray.qsort (λ a b => a.1 < b.1)
  println! "part2 {results[0]!.1}"

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let content <- IO.FS.readFile fname
  let .some thing := parseFile content | println! "parse failed"
  part1 thing
  part2 thing

#eval main ["day5/eg.txt"]
#eval main ["day5/input.txt"]
-- part1 282277027
-- part2 11554135
