import Lean

open Lean RBMap

abbrev Point := Nat × Nat
deriving instance Ord for Prod

abbrev PMap α := RBMap Point α  lexOrd.compare

structure Grid (α : Type) where
  width : Nat
  height : Nat
  data : Array α
deriving Repr

namespace Grid
  def get! [Inhabited α] (g : Grid α) (pt : Point) : α :=
    let (y,x) := pt
    g.data[y*g.width + x]!

  def set! (g : Grid α) (pt : Point) (v : α):=
    let (y,x) := pt
    {g with data := g.data.set! (y*g.width + x) v}

end Grid

-- leaving the \n in the grid, seems harmless?
def load (content : String) : Option (Grid Char) :=
  let lines := content.trim.splitOn "\n"
  let height := lines.length
  let width := lines[0]!.length.succ
  let data := content.toList.toArray
  if data.size = height*width then
    .some ⟨ width, height, data ⟩
  else .none

structure State where
  grid : Grid Char
  dist : PMap Nat
deriving Repr

def init (g : Grid Char) : State := .mk g default

abbrev M := EStateM String State

def neighbors (pt : Point) : M (List (Char × Point)) := do
  let st <- get
  let mut rval := []
  let (r,c) := pt
  if r > 0 then rval := (st.grid.get! (r.pred,c), (r.pred,c)) :: rval
  if r.succ < st.grid.height then rval := (st.grid.get! (r.succ,c), (r.succ,c)) :: rval
  if c > 0 then rval := (st.grid.get! (r,c.pred), (r,c.pred)) :: rval
  if c.succ < st.grid.width then rval := (st.grid.get! (r,c.succ), (r,c.succ)) :: rval
  pure rval

def mark (pt : Point) (d : Nat) : M Unit := do
  modify λ st => {st with dist := st.dist.insert pt d}

structure Edge where
  start : Point
  stop : Point
  len : Nat
deriving Repr, Ord, BEq

partial
def getEdge (start : Point) (prev : Point) (pt : Point) (d : Nat) : M Edge := do
  let ns <- neighbors pt
  let nx := ns.filter (λ (c,pt) => c == '.' && pt != prev)
  match nx with
  | [(_,x)] => do mark pt d; getEdge start pt x d.succ
  | _ => pure ⟨ start, pt, d ⟩

partial
def getEdges (edges : List Edge) : (todo : List (Point × Point))  -> M (List Edge)
| [] => pure edges
| (start,pt) :: rest => do
    let st <- get
    if st.dist.contains pt
    then getEdges edges rest
    else
    mark start 0
    let edge <- getEdge start start pt 1
    if st.dist.contains edge.stop
    then getEdges (edge :: edges) rest
    else
      let ns <- neighbors edge.stop
      let next := ns.filter (λ (c, _) => c == '.')
          |>.map λ (_, pt) => (edge.stop, pt)
      getEdges (edge :: edges) (next ++ rest)

deriving instance Ord for List

structure Status where
  est : Nat
  sunk : Nat
  point : Point
  avail : List Edge
deriving Ord, Repr

abbrev Queue := RBMap Status Unit compare

partial
def run (goal : Point) (q : Queue) (best : Nat) : Nat := Id.run do
  let .some (st,_) := q.max | dbg_trace "MT";best
  let mut q := q.erase st
  if st.point == goal then
    let best := best.max st.sunk
    run goal q best
  else if st.est < best then best
  else
  let avail := st.avail.filter  (λ e => e.start != st.point && e.stop != st.point)
  let ub := avail.foldl (· + ·.len) 0
  for e in st.avail do
    let sunk := st.sunk + e.len
    let est := ub + sunk
    if est > best then
      if e.start == st.point then
        q := q.insert { point := e.stop,  est, sunk, avail } ()
      else if e.stop == st.point then
        q := q.insert { point := e.start, est, sunk, avail } ()
      else
        pure ()
  run goal q best

def part2 (grid : Grid Char) : IO Unit := do
  println! "part2"

  -- Build graph information (extract edges between points for all nexus)
  let start := [((0,1),(1,1))]
  let grid := {grid with data := grid.data.map λ ch => ite (ch == '#') '#' '.'}
  let .ok edges _ := (getEdges [] start).run (init grid) | println! "fail"

  -- run A*
  let max := edges.foldl (· + ·.len) 0
  let st := Status.mk max 0 (0,1) edges
  let result := run (grid.height - 1, grid.width - 3) (.fromList [(st,())] compare) 0

  println! "{result}"


def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .some grid := load input  | println! "parse error"
  println! fname
  part2 grid

#eval main ["day23/eg.txt"]
-- #eval main ["day23/input.txt"]
