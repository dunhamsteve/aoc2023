import Lean

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

namespace Grid
  def get [Inhabited α] (p : Grid α) (r c : Nat) :=
      if h : r < p.height ∧ c < p.width then
        let ix := p.width.succ * r + c
        have : ix < p.data.size := by simp [p.pf,h,grid_ix]
        p.data[ix]
    else default

  def set (p : Grid α) (r c : Nat) (val : α) :=
    if h : r < p.height ∧ c < p.width then
      let ix := p.width.succ * r + c
      have : ix < p.data.size := by simp [p.pf,h,grid_ix]
      { p with data := p.data.set ⟨ix, this⟩  val, pf := by simp [p.pf] }
    else p
  def mark (p : Grid Nat) (r c fl : Nat) :=
    p.set r c (p.get r c ||| fl)
  def check (p : Grid Nat) (r c fl : Nat) : Bool :=
    p.get r c &&& fl > 0

  def dump (p : Grid α) (f : α -> Char) : String :=
    let cs : List Char := p.data.toList.enum.map λ (i,a) => ite (i % p.width.succ = p.width) '\n' (f a)
    cs.asString
end Grid

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

-- North 1
-- East 2
-- South 4
-- West 8

structure Light where
  row : Nat
  col : Nat
  dir : Nat
deriving Repr

def Grid.next (p : Grid Char) (l : Light) (ls : List Light) : List Light := match l with
  | ⟨ r, c, 1 ⟩ => ite (r > 0)  ( ⟨ r.pred, c, 1 ⟩:: ls) ls
  | ⟨ r, c, 2 ⟩ => ite (c.succ < p.width) ( ⟨ r, c.succ, 2⟩ :: ls) ls
  | ⟨ r, c, 4 ⟩ => ite (r.succ < p.height) ( ⟨ r.succ, c, 4 ⟩ :: ls) ls
  | ⟨ r, c, 8 ⟩ => ite (c > 0) (⟨ r, c.pred, 8⟩ :: ls) ls
  | ⟨ _, _, _ ⟩ => dbg_trace "BAD dir {repr l}"; []

-- this is decreasing on number of empty marks and length of ls, but I think that
-- would be a pain to show.
partial
def Grid.stepLight (p : Grid Char) (marks : Grid Nat) : List Light -> Nat
| [] => (marks.data.filter λ x => x > 0).size
| l@⟨r, c, d⟩   :: ls =>
  if marks.check r c d then p.stepLight marks ls
  else
    let marks := mark marks r c d
    match p.get r c, d with
        | '|',  2 | '|', 8 => p.stepLight marks (p.next ⟨r,c,1⟩ (p.next ⟨r,c,4⟩ ls))
        | '-',  1 | '-', 4 => p.stepLight marks (p.next ⟨r,c,2⟩ (p.next ⟨r,c,8⟩ ls))
        | '/',  1 => p.stepLight marks (p.next ⟨r,c,2⟩ ls)
        | '/',  2 => p.stepLight marks (p.next ⟨r,c,1⟩ ls)
        | '/',  8 => p.stepLight marks (p.next ⟨r,c,4⟩ ls)
        | '/',  4 => p.stepLight marks (p.next ⟨r,c,8⟩ ls)
        | '\\', 1 => p.stepLight marks (p.next ⟨r,c,8⟩ ls)
        | '\\', 8 => p.stepLight marks (p.next ⟨r,c,1⟩ ls)
        | '\\', 4 => p.stepLight marks (p.next ⟨r,c,2⟩ ls)
        | '\\', 2 => p.stepLight marks (p.next ⟨r,c,4⟩ ls)

        | '.',  _ | '|',  _ | '-', _ => p.stepLight marks (p.next l ls)
        | _,    _  => dbg_trace "FAIL {repr l} {p.get r c}"; 0

def Grid.zeros (p : Grid α) : Grid Nat :=
  ⟨ mkArray (p.width.succ*p.height) 0, p.width, p.height, by simp [p.pf] ⟩

def best (p : Grid Char) : Nat := Id.run do
  let mut result := 0
  let zeros := p.zeros
  for r in [0:p.height] do
    result := result.max <| p.stepLight zeros [⟨ r, 0, 2 ⟩]
    result := result.max <| p.stepLight zeros [⟨ r, p.width.pred, 8 ⟩]
  for c in [0:p.width] do
    result := result.max <| p.stepLight zeros [⟨ 0, c, 4 ⟩]
    result := result.max <| p.stepLight zeros [⟨ p.height.pred, c, 1 ⟩]
  result

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .some prob := parse input | println! "parse error"
  println! fname
  let result := (prob.stepLight (prob.zeros) [⟨ 0, 0, 2 ⟩])
  println! "part1 {result}"
  println! "part2 {best prob}"

#eval main ["day16/eg.txt"]
#eval main ["day16/input.txt"]
