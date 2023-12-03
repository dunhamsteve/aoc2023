import Lean

namespace List
protected def traverse {F : Type u → Type v} [Applicative F]
    {α : Type} {β : Type u} (f : α → F β) : List α → F (List β)
  | [] => pure []
  | x :: xs => List.cons <$> f x <*> List.traverse f xs
end List

abbrev Draw := Nat × Nat × Nat

inductive Game where
  | mkGame : Nat -> List Draw -> Game
deriving Repr

instance : Add Draw where
  add | (a,b,c), (d,e,f) => (a+d,b+e,c+f)

def max : Draw -> Draw -> Draw
| (a,b,c), (d,e,f) => (a.max d, b.max e, c.max f)

def lte : Draw -> Draw -> Bool
| (a,b,c), (d,e,f) => a <= d && b <= e && c <= f

def parseColor (line : String) : Except String Draw := do
  match line.splitOn with
  | [n, "red"] => .ok ⟨n.toNat!, 0, 0 ⟩
  | [n, "green"] => .ok ⟨0, n.toNat!, 0 ⟩
  | [n, "blue"] => .ok ⟨0, 0, n.toNat!⟩
  | x => .error s!"bad draw {repr x}"

def parseDraw (line : String) : Except String Draw := do
  let parts <- ((line.splitOn ", ").mapM parseColor)
  pure <| parts.foldl max default

def parseGame (line : String) : Except String Game := do
  let [a,b] := line.splitOn ": " | .error s!"No colon in {line}"
  let ["Game", ns] := a.splitOn | .error "No Game"
  let num := ns.toNat!
  let parts <- (b.splitOn "; ").mapM parseDraw
  pure ⟨ num, parts ⟩


def parseFile (content : String) : Except String (List Game) :=
  ((content.splitOn "\n").filter (λ l => l.length > 0)).mapM parseGame

def part1 : List Game -> Nat
| [] => 0
| ⟨ n, parts ⟩ :: rest =>
    let total := parts.foldl _root_.max default
    if lte total (12,13,14)  then  n + part1 rest else part1 rest

def part2 : List Game -> Nat
| [] => 0
| ⟨ _, parts ⟩ :: rest =>
    let (a,b,c) := parts.foldl _root_.max default
    a*b*c+part2 rest

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .ok games := parseFile input | println! "parse Error"

  println! fname
  -- println! (repr games)
  println! (part1 games)
  println! (part2 games)

#eval main ["day2/eg.txt"]
#eval main ["day2/input.txt"]
