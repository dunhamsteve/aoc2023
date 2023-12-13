import Lean
import Lean.Data.Parsec
open Lean.Parsec

abbrev Grid := List String × List String

partial
def transpose (rows : List String) : List String :=
  let rec go (b : List (List Char)) := Id.run do
    let .some heads := b.mapM List.head?  | []
    let .some tails := b.mapM List.tail?  | []
    heads :: go tails
  (go (rows.map String.toList)).map (List.asString)

def mkGrid (chunk : String) : Grid :=
  let lines := chunk.trim.splitOn "\n"
  let tlines := transpose lines
  (lines, tlines)

def parse (content : String) :=
  let chunks := content.trim.splitOn "\n\n"
  dbg_trace "{chunks.length} chunks"
  chunks.map mkGrid


def delta (a b : String) : Nat :=
  let rec go : List Char -> List Char -> Nat -> Nat
    | a :: as, b :: bs, acc => if a != b then go as bs acc.succ else go as bs acc
    | [], _, acc => acc
    | _, [], acc => acc
  go a.toList b.toList 0

def check : List String -> List String -> Nat
| a :: as, b :: bs => delta a b + check as bs
| _ , _ => 0

def countGrid (fuzz : Nat) (left : List String)  (right : List String) (acc : Nat) : Nat := match left, right with
  | _ :: _, a :: as =>
      if check left right == fuzz
        then countGrid fuzz (a :: left) as (acc + left.length)
        else countGrid fuzz (a :: left) as acc
  | [] , a :: as => countGrid fuzz (a :: left) as acc
  | _, [] => acc

def run (fuzz : Nat) (grids : List Grid): IO Unit := do
  let mut total := 0
  for grid in grids do
    let a := countGrid fuzz [] grid.1 0
    total := total + a*100
    let b := countGrid fuzz [] grid.2 0
    total := total + b
  println! total

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let data := parse input
  println! fname
  println! "part1"
  run 0 data
  println! "part2"
  run 1 data

#eval main ["day13/eg.txt"]
#eval main ["day13/input.txt"]
