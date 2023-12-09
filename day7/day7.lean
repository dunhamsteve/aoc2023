import Lean
import Lean.Data.Parsec
open Lean.Parsec

def order := "23456789TJQKA".toList.toArray
def jorder := "J23456789TQKA".toList.toArray

abbrev Hand := Nat × List Nat

def mkHand (part1 : Bool) (cards : String) : Option Hand := do
  let order := ite part1 order jorder
  let kinds <- cards.toList.mapM (λ c => (order.indexOf? c ))
  let mut counts : { ar : Array Int // ar.size = order.size } := ⟨ mkArray order.size 0, by simp ⟩
  for rk in kinds do
    have : rk.val < counts.val.size := by simp [rk.2,counts.2]
    counts := ⟨ counts.1.set ⟨rk.val, this⟩ (counts.1[rk] + 1), by simp [counts.2] ⟩
  let mut type := 0
  let start := ite part1 0 1
  for h:i in [start:counts.1.size] do
    let c := counts.1[i]'h.2
    if c = 5 then type := 6
    if c = 4 then type := 5
    if c = 3 then if type = 1 then type := 4 else type := 3
    if c = 2 then
        if type = 3
        then type := 4
        else if type = 1 then type := 2
        else type := 1
  if ! part1 then
    type := match counts.1.getD 0 0, type with
      | 5, _ => 6
      | 4, _ => 6
      | 3, 1 => 6 | 3, _ => 5
      | 2, 3 => 6 | 2, 1 => 5 | 2, _ => 3
      | 1, 5 => 6 | 1, 3 => 5 | 1, 2 => 4 | 1, 1 => 3 | 1, _ => 1
      | _, _ => type
  .some (type , kinds.map (·.1))

abbrev Line := String × Nat

def parseLine (line : String) : Option Line := do
  let [a,b] := line.splitOn " " | .none
  let n <- b.toNat?
  pure (a,n)

def parseFile (content : String) : Option (List Line) :=
  let lines := content.trim.splitOn "\n"
  lines.mapM parseLine

def lt (a b : Hand) : Bool := a.1 < b.1 || a.1 == b.1 && a.2 < b.2

def run (lines : List Line) (part1 : Bool) : IO Unit := do
  let .some hands := lines.mapM (fun (a,b) => (·,b) <$> mkHand part1 a) | println! "mkHand fail"
  let hands := hands.toArray.qsort λ a b =>  lt a.1 b.1
  let mut p1 := 0
  for h : i in [0:hands.size] do
    p1 := p1 + (hands[i]'h.2).2 * (i + 1)
  println! "{(ite part1 "part1" "part2")} {p1}"

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  println! fname
  let input <- IO.FS.readFile fname
  let .some lines := parseFile input | println! "parse fail"
  run lines true
  run lines false

#eval main ["day7/eg.txt"]
#eval main ["day7/input.txt"]
