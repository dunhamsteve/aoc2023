import Lean

abbrev Point := Nat × Nat

structure Grid (α : Type) where
  width : Nat
  height : Nat
  data : Array α
  start : Point
deriving Repr, Inhabited


-- Unfortunately I can't use these in match statements.
def N := 1
def E := 2
def S := 4
def W := 8

def xlate : Char -> Nat
| '|' => N + S
| '-' => E + W
| 'L' => N + E
| 'J' => N + W
| '7' => W + S
| 'F' => E + S
| 'S' => 15
| _   => 0

namespace Grid
  def get [Inhabited α] (g : Grid α) : Point -> α
  | (r,c) => if r < g.height ∧ c < g.width then g.data[r*g.width+c]! else default

  def set [Inhabited α] (g : Grid α) : Point -> α -> Grid α
  | (r,c), a => if r < g.height ∧ c < g.width
      then let width := g.width; { g with data := (g.data.set! (r*width+c) a) }
      else g

  def neighbors (g : Grid Nat) (pos : Point): (Point × Point) :=
    let (r,c) := pos
    match g.get pos with
    | 5 =>  ((r-1,c), (r+1,c))
    | 10 => ((r,c-1), (r,c+1))
    | 3 =>  ((r-1,c), (r,c+1))
    | 9 =>  ((r-1,c), (r,c-1))
    | 12 => ((r,c-1), (r+1,c))
    | 6 =>  ((r,c+1),(r+1,c))
    | _ => dbg_trace "ERROR neighbors {g.get pos}"; ((0,0),(0,0))


  def guess (g : Grid Nat) (pos : Point) :=
    let (r,c) := pos
    let north := S == S.land (ite (r > 0) (g.get (r.pred, c)) 0)
    let east := W == W.land (g.get (r, c+1))
    let south := N == N.land (g.get (r+1, c))
    let west := E == E.land (ite (c > 0) (g.get (r,c.pred)) 0)

    if north && south then N + S
    else if east && west then E + W
    else if north && east then N + E
    else if north && west then N + W
    else if west && south then W + S
    else if east && south then E + S
    else if north && south then N + S
    else dbg_trace "guess fail {g.get (r.pred,c)}:{north} {g.get (r,c+1)}:{east}  {g.get (r+1,c)}:{south} {g.get (r,c-1)}"; 0

end Grid

-- lean gets slow if I add index proofs? even just 'h.2
def parseGrid (content : String) : Option (Grid Nat) := do
  let lines := ((content.trim.splitOn "\n").map (·.toList))
  let width := lines.head!.length
  let height := lines.length
  let data := lines.join.toArray.map xlate

  let mut start := (0,0)
  let grid : Grid Nat := { width, height, data, start }
  for r in [0:height] do
    for c in [0:width] do
      if grid.get (r,c) == 15 then
        start := (r,c)
        break
  let guess := grid.guess start
  dbg_trace "guessed {guess} at {start}"
  let grid := grid.set start guess
  .some {grid with start := start}

partial
def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let content <- IO.FS.readFile fname
  println! fname
  let .some grid := parseGrid content | println! "parse failed"

  let rec run (marks : Grid Nat) (a b : Point) (step : Nat) : (Nat × Grid Nat) :=
    -- dbg_trace "{step}: a {a} b {b}"
    let marks := (marks.set a step).set b step
    if a == b then (step,marks)
    else let (c,d) := grid.neighbors a
         let (e,f) := grid.neighbors b
         let a := ite (marks.get c = 0) c d
         let b := ite (marks.get e = 0) e f
         run marks a b step.succ

  let marks := {grid with data := mkArray grid.data.size 0}
  let marks := marks.set grid.start 1
  let (a,b) := grid.neighbors grid.start
  let (result,marks) := run marks a b 1

  println! "part1 {result}"

  -- the `inside` state represents inside / outside normally and above/below
  -- when on horizontal lines.  where true is inside / above
  let rec run2 (count : Nat) (inside : Bool) : (data : List Nat) -> (gdata : List Nat) -> Nat
  | 0 :: xs, _ :: ys => run2 (ite inside count.succ count) inside xs ys
  | _ :: xs, y :: ys => match y with
      | 5 | 3 | 9 => run2 count (!inside) xs ys
      | 10 | 6 | 12 => run2 count inside xs ys
      | _=> dbg_trace "illegal state {y} {repr inside}"; 99999999
  | [], [] => count
  | foo, bar => dbg_trace "err {foo} {bar}"; count

  let part2 := run2 0 false marks.data.toList grid.data.toList
  println! "part2 {part2}"


#eval main ["day10/eg.txt"]
#eval main ["day10/eg2.txt"]
#eval main ["day10/eg4.txt"]
#eval main ["day10/eg5.txt"]
#eval main ["day10/eg6.txt"]
#eval main ["day10/input.txt"]
