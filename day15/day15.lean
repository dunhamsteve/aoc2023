import Lean

def hashalg (s : String) : Nat :=
  let rec go
  | c :: cs, acc => go cs (((acc + c.toNat ) * 17) % 256)
  | [], acc => acc
  go s.toList 0

#eval hashalg "HASH"

def part1 (s : String) : IO Unit := do
  let parts := s.trim.splitOn ","
  let result := (parts.map hashalg).foldl (.+.) 0
  println! "part1 {result}"

abbrev Inst := String × Nat
abbrev Box := List (Inst)

def emptyBox : Box := []

def parse (s : String) :=
  match s.splitOn "=" with
  | [lab, n] => (lab, n.toNat!)
  | _ => let lab := (s.splitOn "-")[0]!
         (lab,0)

def dump (boxes : Array Box) : IO Unit := do
  for h : i in [0:boxes.size] do
    let box := boxes[i]'h.2
    if box.length > 0 then
      println! "Box {i} {box}"

def update (inst : Inst) (box : List Inst) := match box, inst with
| (a,b) :: boxes, (l, 0) => if a == l then boxes else (a,b) :: update inst boxes
| (a,b) :: boxes, (l, f) => if a == l then (a,f) :: boxes else (a,b) :: update inst boxes
| [], (_, 0) => []
| [], x => [x]

def part2 (cmds : String) : IO Unit := do
  let instr := (cmds.trim.splitOn ",").map parse
  let mut boxes := mkArray 256 emptyBox
  for inst in instr do
    let ix := hashalg inst.1
    boxes := boxes.set! ix (update inst (boxes.get! ix))

  let mut result := 0
  for h : i in [0:boxes.size] do
    let box := boxes[i]'h.2
    let power := box.enum.foldl (λ acc (ix,f) => acc + (i.succ * ix.succ * f.2)) 0
    result := result + power

  println! "part2 {result}"

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  println! fname
  part1 input
  part2 input

#eval main ["day15/eg.txt"]
#eval main ["day15/input.txt"]
