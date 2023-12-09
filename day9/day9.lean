import Lean
import Lean.Data.Parsec
open Lean.Parsec

def parseLine (s : String) : Option (List Int) := (s.splitOn " ").mapM (·.toInt?)

def diff : List Int -> List Int
| a :: b :: rest => (b - a) :: diff (b :: rest)
| _ => []

def zeros : (List Int) -> Bool
| [] => true
| 0 :: rest => zeros rest
| _ => false

partial
def next (data : List Int) :=
  if zeros data then 0
  else data.getLast! + next (diff data)

partial
def prev (data : List Int) :=
  if zeros data then 0
  else data.head! - prev (diff data)

def sum : List Int -> Int := List.foldl (·+·) 0

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .some rows := (input.trim.splitOn "\n").mapM parseLine
    | println! "parse error"
  let res := rows.map next
  println! fname
  println! "part1 {sum res}"
  let res2 := rows.map prev
  println! "part1 {sum res2}"

#eval main ["day9/eg.txt"]
#eval main ["day9/input.txt"]
