import Lean
import Lean.Data.Parsec
open Lean.Parsec

structure MapEntry where
  dest : Nat
  src  : Nat
  len  : Nat
deriving Repr

abbrev Map := List MapEntry

abbrev Problem := List Nat × List Map

def nums (s : String) := (s.splitOn.filter (·.length > 0)).mapA (·.trim.toNat?)

def parseEntry (part : String) : Option MapEntry := do
  let [dest, src, len] <- nums part | dbg_trace "bad entry {part}";none
  .some ⟨ dest, src, len ⟩

def parseMap (part : List String) : Option Map := do
  let (_ :: part) := part | .none
  part.mapM parseEntry

def parseFile (content : String) : Option Problem := do
  let parts := content.trim.splitOn "\n\n"
  let (first :: rest) := parts | dbg_trace "parse error";.none
  let [_, x] := first.splitOn ": " | .none
  let .some seeds := nums x | none
  let maps <- rest.mapA (λ part => parseMap (part.splitOn "\n"))
  .some (seeds, maps)

def applyEntry (n : Nat) (e : MapEntry) :=
  if n >= e.src && n < e.src + e.len then n + e.dest - e.src else n

def applyMap (n : Nat) : Map -> Nat
| [] => n
| e :: es =>  if n >= e.src && n < e.src + e.len then n + e.dest - e.src else applyMap n es

abbrev Range := Nat × Nat

-- assumes list is sorted
def apply' (r : Range) : List MapEntry -> List Range
| [] => [r]
| ⟨ d,s,l ⟩ :: es =>
    let flen := min (s - r.1) r.2
    let blen := min (r.2 + r.1 - s - l) r.2
    let mlen := r.2 - flen - blen
    let res := if blen = 0 then [] else apply' (r.1+flen+mlen,blen) es
    let res := if mlen = 0 then res else (r.1+flen+d-s,mlen) :: res
    let res := if flen = 0 then res else (r.1,flen) :: res
    res

def apply (ranges : List Range) (entries : List MapEntry) :=
  let entries := (entries.toArray.qsort (·.2 < ·.2)).toList
  (ranges.map (apply' · entries)).join

def mkRanges : List Nat -> Option (List Range)
| a :: b :: rs => mkRanges rs >>= ((a,b) :: ·)
| [] => .some []
| _ => .none

def process (maps : List Map) (ranges : List Range): IO Unit := do
  println! (repr ranges)
  let results := maps.foldl apply ranges
  let results := results.toArray.qsort (λ a b => a.1 < b.1)
  println! results
  println! results[0]!.1

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let content <- IO.FS.readFile fname
  let .some (seeds, maps) := parseFile content | println! "parse failed"
  println! "Part 1"
  process maps (seeds.map (·,1))
  println! "Part 2"
  let .some ranges := mkRanges seeds | println! "odd seeds"
  process maps ranges

#eval main ["day5/eg.txt"]
#eval main ["day5/input.txt"]
