import Lean

structure Grid (α : Type) where
  data : Array α
  width : Nat
  height : Nat
deriving Repr, Inhabited

namespace Grid
  def get [Inhabited α] (p : Grid α) (r c : Nat) := p.data[r*p.width.succ + c]!
  def set (p : Grid α) (r c : Nat) (val : α) :=
      { p with data := p.data.set! (r*p.width.succ + c) val }
  def mark (p : Grid Nat) (r c fl : Nat) :=
    p.set r c (p.get r c ||| fl)
  def check (p : Grid Nat) (r c fl : Nat) : Bool :=
    p.get r c &&& fl > 0

  def dump (p : Grid α) (f : α -> Char) : String :=
    let cs : List Char := p.data.toList.enum.map λ (i,a) => ite (i % p.width.succ = p.width) '\n' (f a)
    cs.asString
end Grid

def parse (content : String) : Option (Grid Char):= do
  let content := content.trim
  let lines := content.splitOn "\n"
  let height := lines.length
  let .some width := lines.head?.map (·.length) | .none
  if height*width.succ - 1 != content.length then
    panic! "*** {content.length} {height} {width} {height*(width+1)}"
  .some ⟨ content.toList.toArray, width, height ⟩

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
  ⟨ mkArray (p.width.succ*p.height) 0, p.width, p.height ⟩

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
