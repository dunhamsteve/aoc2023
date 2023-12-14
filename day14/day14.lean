import Lean
open Lean RBMap

structure Problem where
  data : Array Char
  width : Nat
  height : Nat
deriving Repr, Inhabited

namespace Problem
  def get (p : Problem) (r c : Nat) := p.data[r*p.width.succ + c]!
  def set (p : Problem) (r c : Nat) (val : Char) :=
      { p with data := p.data.set! (r*p.width.succ + c) val }

  def north (p : Problem) : Problem := Id.run do
    let mut rval := p
    for c in [0:rval.width] do
      let mut p := 0
      for r in [0:rval.height] do
        match rval.get r c  with
        | 'O' => do
            rval := rval.set r c '.'
            rval := rval.set p c 'O'
            p := p.succ
        | '#' => p := r.succ
        | _   => ()
    rval

  def south (p : Problem) : Problem := Id.run do
    let mut rval := p
    for c in [0:rval.width] do
      let mut p := 0
      for r in [0:rval.height] do
        match rval.get (rval.height - r - 1) c  with
        | 'O' => do
            rval := rval.set (rval.height - r - 1) c '.'
            rval := rval.set (rval.height - p - 1) c 'O'
            p := p.succ
        | '#' => p := r.succ
        | _   => ()
    rval

  def west (p : Problem) : Problem := Id.run do
    let mut rval := p
    for r in [0:rval.height] do
      let mut p := 0
      for c in [0:rval.width] do
        match rval.get r c  with
        | 'O' => do
            rval := rval.set r c '.'
            rval := rval.set r p 'O'
            p := p.succ
        | '#' => p := c.succ
        | _   => ()
    rval

  def east (p : Problem) : Problem := Id.run do
    let mut rval := p
    for r in [0:rval.height] do
      let mut p := 0
      for c in [0:rval.width] do
        match rval.get r (rval.width - c - 1)  with
        | 'O' => do
            rval := rval.set r (rval.width - c - 1) '.'
            rval := rval.set r (rval.width - p - 1) 'O'
            p := p.succ
        | '#' => p := c.succ
        | _   => ()
    rval

  def weight (p : Problem) : Nat := Id.run do
    let mut total := 0
    for c in [0:p.width] do
      for r in [0:p.height] do
        if p.get r c == 'O' then
          total := total + p.height - r
    total

  def cycle (p : Problem) := p.north.west.south.east

  def dump (p : Problem) : IO Unit := println! p.data.toList.asString ++ "\n"
end Problem

partial
def race (p p2 : Problem) (count : Nat) : Nat :=
  let p := p.cycle
  let p2 := p2.cycle.cycle
  if p.data == p2.data
    then count + 1
    else race p p2 (count + 1)

def process (content : String) : Option Problem := do
  let content := content.trim
  let lines := content.splitOn "\n"
  let height := lines.length
  let .some width := lines.head?.map (·.length) | .none
  if height*width.succ - 1 != content.length then
    panic! "*** {content.length} {height} {width} {height*(width+1)}"
  .some ⟨ content.toList.toArray, width, height ⟩

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  println! fname
  let .some prob := process input | println! "parse error"
  let p2 := prob.north
  println! "part1 {p2.weight}"

  let count := race prob prob 0
  let rem := 1000000000 % count
  println! "{count} {rem}"
  let mut p := prob
  for _ in [0:count+rem] do
    p := p.cycle
  println! "part2 {p.weight}"

#eval main ["day14/eg.txt"]
-- #eval main ["day14/input.txt"]
