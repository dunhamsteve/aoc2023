import Lean
open Lean RBMap

inductive Flag | spring | unknown | empty deriving Repr

structure Line where
  chars : List Flag
  runs : List Nat
deriving Repr, Inhabited

def xlate : Char -> Flag
| '#' => .spring
| '?' => .unknown
| _   => .empty

def parseLine (line : String) : Except String Line := do
  let [a,b] := line.trim.splitOn " " | .error s!"Not two parts {line}"
  let .some nums := (b.splitOn ",").mapM String.toNat? | .error s!"number parse fail on {b}"
  .ok ⟨ a.toList.map xlate, nums ⟩

def parseFile (content : String) : Except String (List Line) := do
  let lines := content.trim.splitOn "\n"
  lines.mapM parseLine

def need : (rs : List Nat) -> Nat
| [r] => r
| r :: rs => r + 1 + need rs
| [] => 0

#eval need [1,2,3]
#eval need []

abbrev M α := StateM (RBMap (Nat × Nat) Nat lexOrd.compare) α

def fits : Nat -> List Flag -> Bool
| .zero, [] => true
| .zero, .unknown :: _ => true
| .zero, .empty :: _ => true
| .succ k, .spring :: cs => fits k cs
| .succ k, .unknown :: cs => fits k cs
| _, _ => false

#eval fits 1 [.unknown]
#eval fits 3 [.spring, .unknown, .spring]

def showFlags : (List Flag) -> List Char
| [] => []
| .unknown :: cs => '?' :: showFlags cs
| .spring :: cs => '#' :: showFlags cs
| .empty :: cs => '.' :: showFlags cs

partial
def solutions (chars : List Flag) (runs : List Nat) (d : Nat) : M Nat := do
  let cl := chars.length
  let rl := runs.length
  let memo (val : Nat) : M Nat := modifyGet λ m => (val, m.insert (cl,rl) val)
  match (<-get).find? (cl,rl) with
  | .some n => pure n
  | .none =>
    match chars with
      | .empty :: cs => solutions cs runs  d.succ >>= memo
      | .spring :: cs => match runs with
          | [] => memo 0
          | r :: rs => if fits r chars
                         then solutions (cs.drop r) rs d.succ >>= memo
                         else memo 0
      | .unknown :: cs => match runs with
          | [] => solutions cs runs d.succ -- TODO just check for springs
          | r :: rs => do let a <- ite (fits r chars) (solutions (cs.drop r) rs d.succ) (pure 0)
                          let b <- solutions cs runs d.succ
                          memo (a + b)
      | [] => memo (ite runs.isEmpty 1 0)
-- termination_by solutions chars runs => chars

def run := λ chars runs => ((solutions chars runs 0).run empty).1

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .ok lines := parseFile input | println! "parse fail"
  println! fname
  let counts := lines.map (λ ⟨ chars, runs ⟩ => run chars runs)
  println! counts.foldl (·+·) 0
  let counts := lines.map (λ ⟨ chars, runs ⟩ =>
        let chars := [Flag.unknown].intercalate [chars,chars,chars,chars,chars]
        let runs := runs ++ runs ++ runs ++ runs ++ runs
        run chars runs)
  println! counts.foldl (·+·) 0

#eval main ["day12/eg.txt"]
#eval main ["day12/input.txt"]
