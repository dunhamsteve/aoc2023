import Lean
import Lean.Data.Parsec

abbrev Star := Nat × Nat

structure Problem where
  stars : List Star
  cols  : Array Nat
  rows  : Array Nat
deriving Repr

def scan (r c :  Nat) (acc : List Star): List Char -> List Star
| [] => acc.reverse
| '\n' :: cs => scan r.succ 0 acc cs
| '#'  :: cs => scan r c.succ ((r,c)::acc) cs
| _    :: cs => scan r c.succ acc cs

def values (gap : Nat)  (acc : Nat): List Bool -> List Nat
| [] => []
| true  :: tl => acc :: values gap (acc + 1) tl
| false :: tl => acc :: values gap (acc + gap) tl

def parse (content : String) (gap : Nat) : Problem := Id.run do
  let stars := scan 0 0 [] content.toList
  let max := stars.foldl (λ (h,w) (r,c) => (h.max r, w.max c)) (0,0)
  let width := max.2.succ
  let height := max.1.succ
  let mut rowFlags := mkArray height false
  let mut colFlags := mkArray width false
  for (r,c) in stars do
    rowFlags := rowFlags.set! r true
    colFlags := colFlags.set! c true
  let rows := (values gap 0 rowFlags.toList).toArray
  let cols := (values gap 0 colFlags.toList).toArray
  return { stars, cols, rows }

def run (prob : Problem) : Nat :=
  let dist := fun (r₁,c₁) (r₂, c₂) => if c₁ < c₂
      then prob.rows[r₂]! -prob.rows[r₁]! + prob.cols[c₂]! - prob.cols[c₁]!
      else prob.rows[r₂]! -prob.rows[r₁]! + prob.cols[c₁]! - prob.cols[c₂]!
  let rec foo :  Nat -> List Star -> Nat
    | acc, [] => acc
    | acc, x :: xs => foo (xs.foldl (λ acc x₂ => acc + dist x x₂) acc) xs
  let res := foo 0 prob.stars
  res

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  println! fname
  let p1 := run (parse input 2)
  let p2 := run (parse input 1000000)
  println! "part1 {p1} part2 {p2}"

#eval main ["day11/eg.txt"]
#eval main ["day11/input.txt"]
