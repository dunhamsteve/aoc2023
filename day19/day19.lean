import Lean
open Lean (RBMap Parsec)
open Lean.Parsec

-- range will be a < x < b
abbrev Range := Nat × Nat

namespace Range
  -- satisfy both ranges, (0,0) for empty (later filtered by isValid)
  def and (a b : Range) : Range :=
    let x := a.1.max b.1
    let y := a.2.min b.2
    if x < y then (x,y) else (0, 0)

  def size : Range -> Nat | (a,b) => b - a - 1

  def all : Range := (0,4001)

end Range

#eval Range.all.size
#eval Range.and (0,2090) (0,4001)

structure Item where
  x : Nat
  m : Nat
  a : Nat
  s : Nat
deriving Repr

inductive Step | step (key : Char) (range : Range) (label : String)
deriving Repr

inductive Rule | rule (name : String) (steps : List Step) (dfl : String)
deriving Repr

def parseNat := String.toNat! <$> many1Chars digit

#eval parseNat.run "123"

def ident := many1Chars asciiLetter

def parseCStep : Parsec Step := do
  let key <- anyChar
  let comp <- pchar '<' <|> pchar '>'
  let num <- parseNat
  skipChar ':'
  let label <- ident
  let cond := ite (comp == '<') (0,num) (num,4001)
  pure (.step key cond label)

#eval parseCStep.run "a<2006:qkq"
#eval parseCStep.run "m>2090:A"

def parseRule : Parsec Rule := do
  let label <- many1Chars asciiLetter
  skipChar '{'
  let steps <- many (attempt parseCStep <* skipChar ',')
  let dfl <- ident
  skipChar '}'
  pure <| .rule label steps.toList dfl

#eval parseRule.run "fdl{m>3467:A,s<1414:A,s>2068:A,A}"

def parseItem : Parsec Item := do
  skipChar '{'
  let x <- skipString "x=" *> parseNat <* skipChar ','
  let m <- skipString "m=" *> parseNat <* skipChar ','
  let a <- skipString "a=" *> parseNat <* skipChar ','
  let s <- skipString "s=" *> parseNat
  skipChar '}'
  pure (Item.mk x m a s)

def parseFile : Parsec (List Rule × List Item) := do
  let rules <- many (parseRule <* ws)
  let items <- many (parseItem <* ws)
  pure (rules.toList, items.toList)


def Step.matches (step : Step) (item : Item) : Bool :=
  let value := match step.1 with | 'x' => item.x | 'm' => item.m | 'a' => item.a | 's' => item.s | _ => 0
  let (a,b) := step.2
  a < value && b > value

abbrev RuleBase := RBMap String Rule compare

partial
def run (rb : RuleBase) (key : String) (item : Item)  : Bool := Id.run do
  if key == "A" then true
  else if key == "R" then false
  else
  let .some (.rule _ steps dfl) := rb.find? key | panic! "can't find {key}"
  for step in steps do
    if step.matches item
      then return run rb step.3 item
  run rb dfl item

def part1 (rb : RuleBase) (items : List Item) : IO Unit := do
  println! "part1"
  let accept := items.filter (run rb "in")
  let result := accept.foldl (λ a i => a + i.x + i.m + i.a + i.s) 0
  println! "part1 {result}"

structure ItemConst where
  x : Range
  m : Range
  a : Range
  s : Range
deriving Repr

-- default is everything
instance : Inhabited ItemConst where
  default := .mk (0,4001) (0,4001) (0,4001) (0,4001)

namespace ItemConst
  def and (l r : ItemConst) : ItemConst :=
    let x := l.x.and r.x
    let m := l.m.and r.m
    let a := l.a.and r.a
    let s := l.s.and r.s
    ⟨ x, m, a, s ⟩

  def size : ItemConst -> Nat | .mk x m a s => x.size * m.size * a.size * s.size
end ItemConst

def Step.constr : Step -> ItemConst
| .step 'x' r _ => ⟨ r, .all, .all, .all ⟩
| .step 'm' r _ => ⟨ .all, r, .all, .all ⟩
| .step 'a' r _ => ⟨ .all, .all, r, .all ⟩
| .step 's' r _ => ⟨ .all, .all, .all, r ⟩
| .step _ _ _ => panic! "bad key"

abbrev Cand := String × ItemConst

def Range.rev : Range -> Range
| (0, v) => (v.pred, 4001)
| (v, 4001) => (0, v.succ)
| r => panic! "Can't reverse {r}"

def Step.deconstr : Step -> ItemConst
| .step 'x' r _ => ⟨ r.rev, .all, .all, .all ⟩
| .step 'm' r _ => ⟨ .all, r.rev, .all, .all ⟩
| .step 'a' r _ => ⟨ .all, .all, r.rev, .all ⟩
| .step 's' r _ => ⟨ .all, .all, .all, r.rev ⟩
| .step _ _ _ => panic! "bad key"

def isValid (cand : Cand) : Bool :=
  let const := cand.2
  const.x.1 + 1 < const.x.2 && const.m.1 + 1 < const.m.2 && const.a.1 + 1 < const.a.2 && const.s.1 + 1 < const.s.2

def runRule (dfl : String) (st : ItemConst) : List Step -> List (String × ItemConst) -> List (String × ItemConst)
| [] , acc => (dfl, st) :: acc
| step :: rest, acc =>  runRule dfl (st.and step.deconstr) rest ((step.3, st.and step.constr ) :: acc)

partial
def process (rb : RuleBase) : (List Cand) -> (List ItemConst)
| [] => []
| ("A", x) :: cs => x :: process rb cs
| ("R", _) :: cs => process rb cs
| (lbl, st) :: cs =>
    match rb.find? lbl with
    | .none =>  panic! "bad label {lbl}"
    | .some (.rule _ steps dfl) =>
        let foo := runRule dfl st steps []
        let foo := foo.filter isValid
        process rb (foo ++ cs)


def calcTotal (table : List (Bool × ItemConst)) (total : Int) : (cands : List ItemConst) -> Int
  | [] => total
  | row :: rest =>
    let xtra := (true, row) :: table.filterMap λ (a,b) =>
            let x := row.and b
            ite (isValid ("",x))  (.some (!a, x)) .none
    let foo : Int := xtra.foldl (λ acc (a,b) => acc + (ite a 1 (-1))*b.size) 0
    calcTotal (xtra ++ table) (total + foo) rest

def part2 (rb : RuleBase) : IO Unit := do
  println! "part2"
  let start := ItemConst.mk (0,4001) (0,4001) (0,4001) (0,4001)

  let cands := process rb [("in",start)] |>.toArray
  println! "got {cands.size} ranges"

  let cands := cands.qsort λ a b => a.size > b.size
  let total := calcTotal [] 0 cands.toList

  println! "done {total}"

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  println! fname
  let data := parseFile.run input
  match data with | .error msg => println! msg | .ok _ => pure ()
  let .ok (rules, items) := data | println! "parse failed"
  let base : RuleBase := rules.foldl (λ a r => a.insert r.1 r) default
  part1 base items
  part2 base

#eval main ["day19/eg.txt"]
#eval main ["day19/input.txt"]
