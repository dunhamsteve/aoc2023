import Lean
import Lean.Data.Parsec
open Lean (RBMap Parsec)
open Lean.Parsec

inductive LR where | left | right deriving Repr, Inhabited

structure Problem where
  steps : Array LR
  graph : RBMap String (String × String) compare
deriving Repr

def pident : Parsec String := (·.toList.asString) <$> many1 asciiLetter <* ws
def kw (s : String) := pstring s <* ws
def ppair : Parsec (String × String) := pure (·,·) <* pchar '(' <*> pident <* pchar ',' <* ws <*> pident <* pchar ')' <* ws
def prule : Parsec (String × String × String ) := pure (·,·) <*> pident <* kw "=" <*> ppair
def plr : Parsec LR := pchar 'L' *> pure .left <|> pchar 'R' *> pure .right

def pfile : Parsec Problem := do
  let steps <- many1 plr
  ws
  let rules <- many1 prule
  let graph := RBMap.fromList rules.toList _
  pure { steps, graph }

partial
def part1 (prob : Problem) (st : String) (pred : String -> Bool) : Option Nat :=
    let rec go (step : Nat) (s : String)  : Option Nat :=
      let ix := step % prob.steps.size
      if pred s then .some step
      else match prob.steps[ix]!, prob.graph.find? s with
      | .left, .some (l,_) => go (step + 1) l
      | .right, .some (_,r) => go (step + 1) r
      | _, .none => .none
    go 0 st

def lcm (n : Nat) (m : Nat) := n * m / (n.gcd m)

partial
def part2 (prob : Problem) : IO Unit :=
  let starts := (prob.graph.toList.map (·.1)).filter λ s => s.endsWith "A"
  let dists := starts.mapA (part1 prob · (λ s => s.endsWith "Z"))
  match dists with
  | .none => println! "blah"
  | .some dists => println! (dists.foldl lcm 1)

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .ok prob := pfile.run input | println! "parse error"
  println! fname
  let p1 := part1 prob "AAA" (·  == "ZZZ")
  println! "part1 {p1}"
  part2 prob

#eval main ["day8/eg.txt"]
#eval main ["day8/input.txt"]
