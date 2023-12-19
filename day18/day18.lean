import Lean
open Lean (RBMap Parsec)
open Lean.Parsec

def hexToNat (s : String) : Nat :=
  let rec go (acc : Nat)
  | v :: cs => let p := v.toNat; go (acc * 16 + ite (p > 96) (p-87) (p - 48)) cs
  | [] => acc
  go 0 s.toList

#eval hexToNat "1a"
#eval hexToNat "abcd"

inductive Dir | left | right | up | down deriving Repr, Inhabited

def pDir := pchar 'R' *> pure Dir.right
       <|>  pchar 'L' *> pure Dir.left
       <|>  pchar 'U' *> pure Dir.up
       <|>  pchar 'D' *> pure Dir.down

def pcolor := pchar '#' *> many hexDigit

def pnat := (λ x => x.toList.asString.toNat!) <$> many digit

structure Instr where
  dir : Dir
  len : Nat
  color : Nat
deriving Repr

def pline : Parsec Instr := do
  let d <- pDir <* ws
  let p <- pnat <* ws
  let _ <- pchar '('
  let color <- (λ (x : Array Char) => hexToNat x.toList.asString) <$> pcolor
  let _ <- pchar ')'
  -- ws
  pure ⟨ d, p, color ⟩

def pfile : Parsec (Array Instr) := many (pline <* ws)

structure Line where
  dir : Nat
  start : Int
  stop  : Int
  pos   : Int
deriving Repr

-- R D L U
-- 0 1 2 3
def getLines (r v : Int) : List (Nat × Nat) -> List Line
  | [] => []
  | (dir,len) :: is =>
      match dir with
      | 0 => let c' := v + len; ⟨ dir, v, c', r⟩  :: getLines r c' is
      | 2 => let c' := v - len; ⟨ dir, c', v, r⟩  :: getLines r c' is

      | 3 => let r' := r - len; ⟨ dir, r', r, v⟩  :: getLines r' v is
      | 1 => let r' := r + len; ⟨ dir, r, r', v⟩  :: getLines r' v is
      | _ => dbg_trace "bad dir {dir}"; []

def legend := λ p => match p with | 1 => '#' | 2 => '*' | 0 => '.' | _ => '?'

def xlate : Dir -> Nat
| .right => 0
| .down => 1
| .left => 2
| .up => 3

def run (sizes : List (Nat × Nat)) : IO Unit := do
  let lines := getLines 0 0 sizes
  let vlines := lines.filter (λ l => l.dir == 1 || l.dir == 3)
  let vpoints := (vlines.map (·.start) ++ vlines.map (·.stop))
      |>.eraseDups.toArray.qsort (·<·)

  let height := vpoints.size

  let mut result : Int := 0
  -- each row
  let vlines := vlines.toArray.qsort (λ l₁ l₂ => l₁.pos < l₂.pos)
  for r in [0:height] do
    let mut prev : Bool × Option Int := (false, .none) -- outside
    let start := vpoints[r]!

    let stop := ite (r == height - 1) (start + 1) vpoints[r+1]!

    let vs := stop - start
    -- println! "Row {r} {start} {stop} {stop-start}"
    for l in vlines do
      -- count squares row by row
      if l.start < stop && start < l.stop then  -- overlap
        -- println! "r{r} {start}/{stop} {repr l}"
        match prev with
        -- starting. We have top if the starts match and we didn't have top coming in
        | (top, .none) => prev := (l.start == start && !top, .some l.pos)
        -- ending
        | (top, .some p) => do
              -- if we don't have top, the previous band is counting it
              let vs' := ite top (vs + 1) vs
              -- we do have the right bar
              let foo := (l.pos - p + 1)*vs'
              result := result + foo
              -- println! "end [{top}] {p}/{l.pos} h={vs'} w={l.pos - p + 1} {foo}"
              prev := (l.start == start && !top, .none)
      else if start == l.stop then
        -- line above us toggles top
        match prev with
        | (top, .none) => prev := (!top, .none)
        | (top, .some p) => do

              let vs' := ite top (vs + 1) vs
              -- println! "adj [{top}] {p}/{l.pos} h={vs'} w={l.pos - p} {!top}"
              -- top right corner counted twice when going False -> True
              result := result + (l.pos - p)*vs' - (ite top 0 1)
              prev := (!top, .some l.pos)

  println! result

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let data := pfile.run input
  println! fname
  let .ok data := data | println! "bail"
  let is := data.toList
  println! "part1"
  run <| is.map λi => (xlate i.dir, i.len)
  println! "part2"
  run <| is.map λi => (i.color % 16, i.color / 16)

#eval main ["day18/eg.txt"]
#eval main ["day18/input.txt"]
