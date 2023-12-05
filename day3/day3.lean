import Lean
import Lean.Data.Parsec
open Lean.Parsec

structure Grid (α : Type) where
  width : Nat
  height : Nat
  data : Array (Array α)
deriving Repr

-- default char is 'A' ?
#eval (default : Char)

def parseGrid (content : String) : Grid Char :=
  let data := ((content.trim.splitOn "\n").map (·.toList.toArray)).toArray
  let width := data[0]!.size
  let height := data.size
  { width, height, data }

structure Number where
  start : Nat
  stop : Nat
  value : Nat
deriving Repr
partial -- fixme
def numbers (g : Grid Char) (y : Nat) :=
  let l := g.width
  let rec go (x : List Char) (start : Nat) : List Number :=
      match x with
      | c :: cs => if c.isDigit
                      then let (front, back) := List.span Char.isDigit x
                           let stop := start + (front.length)
                           ⟨ start, stop, front.asString.toNat! ⟩ :: go back stop
                      else go cs (start + 1)
      | [] => []
  go g.data[y]!.toList 0

#eval numbers ⟨ 10, 1, #[ "..123...456..".toList.toArray ] ⟩ 0

def isPart (g : Grid Char) (y : Nat) (n : Number) : Bool :=
  let ⟨ s, e, _ ⟩ := n
  let check (y : Nat) :=
      (g.data[y]!.toSubarray s.pred e.succ).any (λ c => !c.isDigit && c != '.' )
  check y.pred || check y || (y.succ < g.height && check y.succ)

def sum (ns : List Nat) : Nat := ns.foldl (·+·) 0

def part1 (g : Grid Char) : IO Unit := do
  let mut total := 0
  for y in [0:g.height] do
    for n in numbers g y do
      if isPart g y n then
        total := total + n.3

  println! "total {total}"

def gears (g : Grid Char) (y : Nat) :=
  -- for now we forget we have Fin
  let candidates := ((g.data[y]!.mapIdx (λ ix c => (ix.1,c))).toList.filter (λ x => x.2 = '*')).map (·.1)
  let a := if y > 0 then numbers g y.pred else []
  let b := numbers g y
  let c := if y.succ < g.height then numbers g y.succ else []
  let ratio x :=
    let nums := (a ++ b ++ c).filter (λ ⟨ s,e,_⟩  => s.pred <= x && e >= x)
    match nums with
    | [a,b] => a.value * b.value
    | _ => 0
  sum (candidates.map ratio)

def part2 (g : Grid Char) : IO Unit := do
  let mut total := 0
  for y in [0:g.height] do
    total := total + gears g y
  println! "part2 {total}"

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let grid := parseGrid input
  println! fname
  part1 grid
  part2 grid

#eval main ["day3/eg.txt"]
#eval main ["day3/input.txt"]
