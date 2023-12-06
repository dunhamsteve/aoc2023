import Lean
import Lean.Data.Parsec
open Lean.Parsec

abbrev Problem := List (Nat × Nat)

def pNums (line : String) : Except String (List Nat) := do
  let [_,line] := line.splitOn ": " | .error "expected two parts"
  .ok ((line.splitOn " ").filterMap (·.toNat?))

def parse (content : String) : Except String Problem := do
  let [a,b] := content.trim.splitOn "\n" | .error "expected two lines"
  let times <- pNums a
  let dists <- pNums b
  .ok (times.zip dists)

def parse2 (content : String) : Option (Nat × Nat) := do
  let [a,b] := content.trim.splitOn "\n" | .none
  let time <- (a.toList.filter (·.isDigit)).asString.toNat?
  let dist <- (b.toList.filter (·.isDigit)).asString.toNat?
  .some (time,dist)

def part1 (prob : Problem) : IO Unit := do
  let mut part1 := 1
  for (time,dist) in prob do
    let mut count := 0
    for t in [0:time] do
      if t * (time - t) > dist then count := count + 1
    part1 := part1 * count
  println! "part1 {part1}"

def part2 (prob : Nat × Nat) : IO Unit := do
  let (time,dist) := prob
  let t := time.toFloat
  let d := dist.toFloat
  let x := (t * t - 4 * d).sqrt
  let start := (t - x)/2
  let stop := (t + x)/2

  let s := start.ceil.toUInt64.toNat
  let e := stop.ceil.toUInt64.toNat
  println! "part2 {e - s}"

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let content <- IO.FS.readFile fname
  match parse content with
  | .error msg => println! msg
  | .ok prob => do
      part1 prob
  let .some p2 := parse2 content | println! "parse2 failed"
  part2 p2

#eval main ["day6/eg.txt"]
#eval main ["day6/input.txt"]
