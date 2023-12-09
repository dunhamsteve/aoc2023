import Lean
import Lean.Data.Parsec
open Lean.Parsec

def parseLine (s : String) : Option (List Int) := (s.splitOn " ").mapM (·.toInt?)

def diff : List Int -> List Int
| a :: b :: rest => (b - a) :: diff (b :: rest)
| _ => []

theorem diff_smaller : ∀ (xs : List Int) (n : Nat),
  (h : xs.length = .succ n) -> (diff xs).length < xs.length := by
  intros xs
  induction xs
  intro n h
  contradiction
  rename_i tail ih
  intro n h
  match tail with
  | .nil => apply Nat.zero_lt_succ
  | x :: tail =>
          apply Nat.succ_lt_succ
          apply ih tail.length rfl

def zeros : (List Int) -> Bool
| [] => true
| 0 :: rest => zeros rest
| _ => false

def next (data : List Int) :=
  if zeros data then 0
  else match h : data with
  | x :: xs => data.getLast (by simp [h]) + next (diff data)
  | _ => 0
termination_by next data => data.length
decreasing_by apply diff_smaller; rw [h]; rfl

def prev (data : List Int) :=
  if zeros data then 0
  else match h : data with
  | x :: xs => data.head (by simp [h]) - prev (diff data)
  | _ => 0
termination_by prev data => data.length
decreasing_by apply diff_smaller; rw [h]; rfl

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
