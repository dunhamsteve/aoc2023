import Lean

open Lean RBMap

structure Grid (α : Type) where
  data : Array α
  width : Nat
  height : Nat
deriving Repr

instance [Inhabited α ] : Inhabited (Grid α) where
  default := ⟨ mkArray 0 default , 0, 0 ⟩

abbrev Point := Int × Int

instance : Ord Point where
  compare := lexOrd.compare

namespace Grid
  def get [Inhabited α] (p : Grid α) (pt : Point) :=
      let (r,c) := pt
      let r' := r % p.height
      let r' := ite (r' < 0) (r' + p.height) r'
      let c' := c % p.width
      let c' := ite (c' < 0) (c' + p.width) c'
      let ix := p.width * r' + c'
      p.data[ix.toNat]!

  def set (p : Grid α) (pt : Point) (val : α) :=
    let (r,c) := pt
    if r < p.height ∧ c < p.width then
      let ix := p.width * r + c |>.toNat
      let data := p.data
      { p with data := data.set! ix val }
    else p

  def dump [Inhabited α] (p : Grid α) (f : α -> Char) : String := Id.run do
    let mut rv := ""
    for r in [0:p.height] do
      for c in [0:p.width] do
        rv := rv.append (f (p.get (r,c))).toString
      rv := rv.append "\n"
    rv

  def map (p : Grid α) (f : α -> β ) : Grid β :=
    let data := p.data.map f
    ⟨ data, p.width, p.height ⟩

end Grid

def parse (content : String) : Option (Grid Char):= do
  let content := content
  let lines := content.trim.splitOn "\n" |>.map String.trim
  let height := lines.length
  let .some width := lines.head?.map (·.length) | .none
  let data := "".intercalate lines |>.toList.toArray
  if data.size = width*height then
    .some ⟨ data, width, height ⟩
  else
    dbg_trace "*** {content.length} {height} {width} {height*(width+1)}"
    .none

def step (g : Grid Char) : Grid Char := Id.run do
  let mut g2 := g
  for r in [0:g.height] do
    for c in [0:g.width] do
      let ch := if g.get (r,c) == '#' then '#'
        else if r > 0 && g.get (r-1,c) == 'O'
        || r + 1 < g.height && g.get (r+1,c) == 'O'
        || c > 0 && g.get (r,c-1) == 'O'
        || c + 1 < g.width && g.get (r,c+1) == 'O'
        then 'O' else '.'
      g2 := g2.set (r,c) ch
  g2

inductive OptGrid | grid (g : Grid Char) | sat

structure MegaGrid where
  base : Grid Char
  -- using grid of grids to mark some done
  grids : RBMap Point (Grid Char) compare

def Grid.count (g : Grid Char) :=
  let rec go acc : Nat -> Nat
  | .succ k => go (ite (g.data[k]! = 'O') (acc+1) acc) k
  | .zero => acc
  go 0 g.data.size

def part1 (grid : Grid Char) (count : Nat) : IO Unit := do
  let .some ix := grid.data.findIdx? (· == 'S') | println! "can't find S"
  let start : Point := (ix / grid.width , ix % grid.width)
  println! "st {start}"
  let grid := grid.set start 'O'

  println! "wh {(grid.width, grid.height)}"

  let mut tmp := grid
  for _ in [0:count] do
    tmp := step tmp

  println! "P1 {count} total {tmp.count}"

-- partial
def part2 (grid : Grid Char) : IO Unit := do
  -- It's in the middle..
  let .some ix := grid.data.findIdx? (· == 'S') | println! "can't find S"
  let start : Point := (ix / grid.width , ix % grid.width)
  println! "start {start}"
  let grid := grid.set start '.'

  println! "dimensions {(grid.width, grid.height)}"

  -- run a 5x5 grid with enough steps to reach the sides

  let size := grid.width
  let n := 2
  let scale := 2*4 + 1

  let mut newGrid := Grid.mk (mkArray (grid.width*grid.height*scale*scale) '.')  (grid.height * scale) (grid.width * scale)
  for r in [0:newGrid.height] do
    for c in [0:newGrid.width] do
      newGrid := newGrid.set (r,c) (grid.get (r % grid.width, c % grid.width))

  let start := (start.1 + size*n, start.2 + size * n)
  newGrid := newGrid.set start 'O'

  let mut tmp := newGrid

  for _ in [0:start.1.toNat] do
    tmp := step tmp

  -- We fetch individual chunks for parts of the diamond. This just a growing diamond, but the frontier is consistently fuzzy.
  let getChunk (y x : Nat)  :=  Id.run do
    let mut rval : Nat := 0
    for r in [y*size:y.succ*size] do
      for c in [x*size:x.succ*size] do
        if tmp.get (r,c) == 'O' then
          rval := rval + 1
    rval
  let n := 202300
  let solution :=
      n*(getChunk 1 0)
    + n*(getChunk 0 3)
    + n*(getChunk 3 0)
    + n*(getChunk 3 4)
    + n.pred*(getChunk 1 1)
    + n.pred*(getChunk 1 3)
    + n.pred*(getChunk 3 1)
    + n.pred*(getChunk 3 3)
    + n*n*(getChunk 2 1)
    + n.pred*n.pred*(getChunk 2 2)
    + getChunk 0 2
    + getChunk 2 0
    + getChunk 2 4
    + getChunk 4 2

  -- Part 2 is 202300 + 1 + 202300 blocks wide
  println! "part 2 {solution}"

def main(args : List String) : IO Unit := do
  let fname :: count :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .some grid := parse input | println! "parse failed"
  let .some count := count.toNat? | println! "{count} not a number"
  println! fname
  part1 grid count
  part2 grid


#eval main ["day21/eg.txt", "6"]
-- #eval main ["day21/input.txt", "64"]
