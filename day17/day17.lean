import Lean

open Lean RBMap

structure Grid (α : Type) where
  data : Array α
  width : Nat
  height : Nat
  pf : data.size = width.succ * height
deriving Repr

instance [Inhabited α ] : Inhabited (Grid α) where
  default := ⟨ mkArray 0 default , 0, 0, rfl ⟩

theorem grid_ix {r c width height : Nat} (h : r < height ∧ c < width) : width.succ * r + c < width.succ*height := by
  --let ix := r*width.succ + c
  have h2 : width.succ * r.succ ≤width.succ * height := by
    apply Nat.mul_le_mul_left
    exact h.1
  apply Nat.lt_of_lt_of_le _ h2
  rw [Nat.mul_succ]
  apply Nat.add_lt_add_left
  apply Nat.lt_trans h.2
  apply Nat.lt_succ_self

abbrev Point := Nat × Nat

instance : Ord Point where
  compare := lexOrd.compare

inductive Dir | vert | horiz | none
deriving Repr, BEq, Ord

structure Node where
  pt : Point
  fr : Dir
deriving Repr, Ord, BEq

namespace Grid
  def get [Inhabited α] (p : Grid α) (pt : Point) :=
      let (r,c) := pt
      if h : r < p.height ∧ c < p.width then
        let ix := p.width.succ * r + c
        have : ix < p.data.size := by simp [p.pf,h,grid_ix]
        p.data[ix]
    else default

  def set (p : Grid α) (pt : Point) (val : α) :=
    let (r,c) := pt
    if h : r < p.height ∧ c < p.width then
      let ix := p.width.succ * r + c
      have : ix < p.data.size := by simp [p.pf,h,grid_ix]
      { p with data := p.data.set ⟨ix, this⟩  val, pf := by simp [p.pf] }
    else p

  def dump (p : Grid α) (f : α -> Char) : String :=
    let cs : List Char := p.data.toList.enum.map λ (i,a) => ite (i % p.width.succ = p.width) '\n' (f a)
    cs.asString

  def map (p : Grid α) (f : α -> β ) : Grid β :=
    let data := p.data.map f
    ⟨ data, p.width, p.height, by sorry ⟩ -- TODO

end Grid

abbrev Problem := Grid Nat × Nat × Nat

def neighbors (p : Grid Nat) (node : Node) (min max : Nat) := Id.run do
    let mut rval : List (Node × Nat) := []
    let ⟨ (r,c), fr ⟩  := node
    let mut w := 0
    if fr != .vert then
      w := 0

      for d in [1:max] do
        if r < d then break
        let pt := (r-d, c)
        w := w + p.get pt
        if d >= min then
          rval := (⟨pt, .vert⟩, w)  :: rval
      w := 0
      for d in [1:max] do
        if r + d < p.height then
          let pt := (r + d, c)
          w := w + p.get pt
          if d >= min then
            rval := (⟨pt, .vert⟩, w) :: rval
    if fr != .horiz then
      w := 0
      for d in [1:max] do
        if c < d then break
        let pt := (r, c-d)
        w := w + p.get pt
        if d >= min then
          rval := (⟨pt, .horiz⟩ , w) :: rval
      w := 0
      for d in [1:max] do
        if c + d < p.width then
          let pt := (r, c + d)
          w := w + p.get pt
          if d >= min then
            rval := (⟨pt, .horiz⟩ , w) :: rval
    return rval

def parse (content : String) : Option (Grid Char):= do
  let content := content
  let lines := content.trim.splitOn "\n"
  let height := lines.length
  let .some width := lines.head?.map (·.length) | .none
  let data := content.toList.toArray
  if h : data.size = width.succ*height then
    .some ⟨ content.toList.toArray, width, height, h ⟩
  else
    dbg_trace "*** {content.length} {height} {width} {height*(width+1)}"
    .none

structure State where
  cost : Nat
  loc  : Node

deriving Repr, Ord, BEq

abbrev Queue := RBMap State Unit compare

partial
def run (p : Problem) (dist : RBMap Node Nat compare) (q : Queue) : Option Nat := Id.run do
  let (g, min, max) := p
  let .some (next ,_) := q.min | .none
  let mut q := q.erase next
  let mut dist := dist
  let ⟨ cost, thisNode ⟩ := next
  if thisNode.1 == (g.height.pred, g.width.pred) then return .some cost
  else for (n, delta) in neighbors g thisNode min max do
    let cost := cost + delta
    -- don't bother if we've already been there at the same cost
    let isCand := match dist.find? n with | .none => true | .some d => d >= cost
    if isCand then
      q := q.insert ⟨ cost, n ⟩ ()
      dist := dist.insert n cost
  run p dist q

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  println! fname
  let .some cgrid := parse input | println! "parse failed"
  let ngrid := cgrid.map (λ c => c.toNat - 48)

  println! "{ngrid.height} {ngrid.width}"
  let st : Node := ⟨ (0,0), .none ⟩
  let q : Queue := fromList [(⟨0, st ⟩ ,())] compare

  let result := run (ngrid, 1, 4) empty q
  println! "part1 {result}"

  let result := run (ngrid, 4, 11) empty q
  println! "part2 {result}"

#eval main ["day17/eg.txt"]
-- This runs out of stack in editor.
-- #eval main ["day17/input.txt"]
