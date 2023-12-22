import Lean

open Lean RBMap

abbrev Point := (Nat × Nat × Nat)
abbrev Range := Nat × Nat

instance : Ord Range where
  compare := lexOrd.compare

structure Brick where
  x : Range
  y : Range
  z : Range
deriving Repr, Inhabited

def trace [Repr α] (s : String) (a : α) : α := dbg_trace "{s} {repr a}"; a

-- need to sort how how < works in Type (if at all)
def ltBrick (a b : Brick) : Bool :=
    if a.3 == b.3 then
      if a.1 == b.1 then compare a.2 b.2 == .lt else compare a.1 b.1 == .lt
    else compare a.3 b.3 == .lt

def mkBrick : Point -> Point -> Brick
| (x₁, y₁, z₁), (x₂, y₂, z₂ ) => .mk (x₁, x₂) (y₁, y₂) (z₁, z₂)

-- ## Parsing

def Range.isect (a b : Range) := a.1 <= b.2 && b.1 <= a.2

def Brick.zto (z : Nat) : Brick -> Brick
| .mk x y (z₁, z₂) => .mk x y (z, z + z₂ - z₁)

def Brick.isectxy (a b : Brick) :=
  a.1.isect b.1 && a.2.isect b.2


def parseBrick (line : String) : Option Brick := do
  let [a,b] := line.splitOn "~" | .none
  let .some [x₁, y₁, z₁] := a.splitOn "," |>.mapM String.toNat? | .none
  let .some [x₂, y₂, z₂] := b.splitOn "," |>.mapM String.toNat? | .none
  .some (.mk (x₁, x₂) (y₁, y₂) (z₁, z₂))

#eval parseBrick "1,0,1~1,2,1"

def parseFile (content: String) : Option (Array Brick) := do
  let lines := content.trim.splitOn "\n"
  let bricks <- lines.mapM parseBrick
  bricks.toArray.qsort ltBrick

-- ## Part1

abbrev Pile := List Brick

def findZ (b : Brick) : Pile -> Nat
| [] => 1
| x :: xs => if b.isectxy x
    then x.z.2.succ
    else findZ b xs

def Pile.insert (b : Brick) : Pile -> Pile
| [] => [b]
| x :: xs => if b.z.2 > x.z.2
    then b :: x :: xs
    else x :: insert b xs

def dropAll : Pile -> Pile -> Pile
| [], acc => acc
| b :: bs, acc =>
    let z := findZ b acc
    let b := { b with z := (z, b.z.2 - b.z.1 + z) }
    let acc := acc.insert b
    dropAll bs acc

def Brick.support (b : Brick) : List (Nat × Brick) -> List Nat
| [] => []
| (ix, x) :: rest =>
    if x.z.2.succ = b.z.1 && b.isectxy x
    then ix :: b.support rest
    -- else if x.z.1 > b.z.1 then []
    else b.support rest

def supports : List (Nat × Brick) -> List (List Nat)
| [] => []
| (_, b) :: rest => b.support rest :: supports rest

structure Node where
  ix : Nat
  b : Brick
  supports : List Nat
  atop : List Nat
deriving Repr, Inhabited

def buildTable (bricks : Array Brick) : Array Node := Id.run do
  let bricks := bricks.qsort λ a b => a.z.2 < b.z.2
  let bricks := dropAll bricks.toList []
  let below := supports bricks.enum
  let mut table := bricks.zip below
      |>.enum.map (λ (ix, b,atop) => Node.mk ix b [] atop)
      |>.toArray

  for node in table do
    for ix in node.atop do
      let n := table.get! ix
      table := table.set! ix {n with supports := node.ix :: n.supports }
  return table

def part1 (bricks : Array Brick) : IO Unit := do
  let table := buildTable bricks
  let cand := table.toList.filter λ n =>
    n.supports == [] || n.supports.all λ ix => table[ix]!.atop.length > 1
  println! "part1 {cand.length}"

structure State where
  table    : Array Node
  dropped  : RBMap Nat Unit compare
abbrev DropM := StateM State

def dropNode (ix : Nat) : DropM Unit :=
  modify λ st => {st with dropped := st.dropped.insert ix () }

def supported (ix : Nat) : DropM Bool := do
  let st <- get
  let .some node := st.table[ix]? | pure true
  if node.b.z.1 == 1 then return true
  let rval := node.atop.any λ ix => !(st.dropped.contains ix)
  pure rval

def disintegrate (sorted : Array Node) (boom : Nat) : DropM Nat := do
  dropNode boom
  for n in sorted do
    if !(<-supported n.ix) then
      dropNode n.ix
  let st <- get
  -- don't count disintegrated one
  pure st.dropped.size.pred

def part2 (bricks : Array Brick) : IO Unit := do
  println! "part2"
  let table := buildTable bricks

  -- process bottom-up to so supports have already fallen
  let sorted := table.qsort λ x y =>
    let a := x.b.z
    let b := y.b.z
    if a.2 == b.2 then a.1 < b.1 else a.2 < b.2

  let mut total := 0
  for n in table do
    let count : Nat := disintegrate sorted n.ix |>.run' ⟨ table, default ⟩
    total := total + count

  println! "part2 {total}"


-- ## Main

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .some bricks := parseFile input | println! "parse error"
  println! fname
  part1 bricks
  part2 bricks

#eval main ["day22/eg.txt"]
#eval main ["day22/input.txt"]
