import Lean

open Lean RBMap

abbrev Point := Nat × Nat

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

abbrev Dist := RBMap Point Nat lexOrd.compare

structure State where
  grid : Grid Char
  dist : Dist
  prev : Point

def init (g : Grid Char) : State := .mk g default default

-- todo is a list of points to consider
abbrev M := EStateM String State

-- follow path and then do arrows

def neighbors (pt : Point) : M (List (Char × Point)) := do
  let st <- get
  let mut rval := []
  let (r,c) := pt
  if r > 0 then rval := (st.grid.get! (r.pred,c), (r.pred,c)) :: rval
  if r.succ < st.grid.height then rval := (st.grid.get! (r.succ,c), (r.succ,c)) :: rval
  if c > 0 then rval := (st.grid.get! (r,c.pred), (r,c.pred)) :: rval
  if c.succ < st.grid.width then rval := (st.grid.get! (r,c.succ), (r,c.succ)) :: rval
  pure rval

def arrows (pt : Point) : M (List Point) := do
  let st <- get
  let mut rval := []
  let (r,c) := pt
  if r > 0 && '^' == st.grid.get! (r.pred,c)
    then rval := (r.pred.pred,c) :: rval
  if r.succ < st.grid.height  && 'v' == st.grid.get! (r.succ,c)
    then rval := (r.succ.succ,c) :: rval
  if c > 0 && '<' == st.grid.get! (r,c.pred)
    then rval := (r,c.pred.pred) :: rval
  if c.succ < st.grid.width  && '>' == st.grid.get! (r,c.succ)
    then rval := (r,c.succ.succ) :: rval
  pure rval

def mark (pt : Point) (d : Nat) : M Unit := do
  modify λ st => {st with dist := st.dist.insert pt d}

partial
def trace (pt : Point) (prev : Point) (d : Nat) : M (List (Point × Nat)) := do
  mark pt d
  let nx :=(<-neighbors pt).filter
     λ (ch, p) => ch == '.' && p != prev
  match nx with
  | [(_,x)] => trace x pt d.succ
  | [] => do
    let next <- arrows pt
    pure <| next.map (· , d.succ.succ)
    -- shouldn't happen
  | _ => dbg_trace "fork {pt}"; pure []

def check (pt : Point) (d : Nat) : M Bool := do
  let st <- get
  pure <| match st.dist.find? pt with
    | .none => true
    | .some x => x < d

partial
def run : (todo : List (Point × Nat)) -> M Nat
| [] => do
    let st <- get
    let exit := (st.grid.height-1,st.grid.width-2)
    dbg_trace "{exit}"
    pure <| st.dist.findD (st.grid.height-1,st.grid.width-3) 0
| (pt, d) :: rest => do
    if <-check pt d then
    let next <- trace pt pt d
    run (next ++ rest)
    else run rest

def part1 (grid : Grid Char) : IO Unit := do
  println! "part1 {grid.height} {grid.width}"
    let foo := (run [((0,1),0)]).run (init grid)
    let .ok result _ := foo | println! "analyze failed"
    println! (repr result)

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .some grid := load input  | println! "parse error"
  println! fname
  part1 grid


#eval main ["day23/eg.txt"]
--  #eval main ["day23/input.txt"]
