import Lean

open Lean RBMap Std Parsec

inductive Node
| flip (mem : Bool) (out : List String)
| nand  (input : RBMap String Bool compare) (out : List String)
deriving Repr

abbrev Signals := Queue (String × String × Bool)

inductive Stmt
| flip (name : String) (out : List String)
| and (name : String) (out : List String)
| broadcast (out : List String)
deriving Repr

instance : Repr Signals where
  reprPrec s _ := repr s.toArray

abbrev Nodes := RBMap String Node compare

structure Machine where
  nodes : Nodes
  broadcast : List String
  low : Nat
  high : Nat
  halt : Bool
  epoch : Nat    -- this was for logging
  probe : String -- halt when this sends a true
deriving Repr, Inhabited

def ident := manyChars asciiLetter

def pNames := do
  let n <- ident
  let ns <- many (skipChar ',' <* ws *> ident)
  pure (n :: ns.toList)

def pBroadcast : Parsec Stmt := do
  pstring "broadcaster" *> ws
  pstring "->" *> ws
  let ns <- pNames
  pure (.broadcast ns)

def pFlip := pure Stmt.flip <* pchar '%' <*> ident <* ws <* pstring "->" <* ws <*> pNames
def pAnd := pure Stmt.and <* pchar '&' <*> ident <* ws <* pstring "->" <* ws <*> pNames
def pStmt := pBroadcast <|> pFlip <|> pAnd
def pFile := many (pStmt <* ws)

def Nodes.initAnd (n : String) (ns : Nodes) (nm : String) : Nodes :=
  match ns.find? nm with
  | .some (.nand i o) => ns.insert nm (.nand (i.insert n false) o)
  | _ => ns

def parse (content : String) : IO Machine := do
  let mut rval : Machine := default
  let .ok stmts := pFile.run content
    | println! "parse failed"; pure default

  -- need to build nodes and initialize & nodes
  for stmt in stmts do
    match stmt with
    | .broadcast ns => rval := { rval with broadcast := ns }
    | .flip nm out => rval := {rval with nodes := rval.nodes.insert nm (.flip false out)}
    | .and nm out => rval := {rval with nodes := rval.nodes.insert nm (.nand default out)}

  -- initialize and memory
  for stmt in stmts do
    match stmt with
    | .broadcast ns => rval := { rval with nodes := ns.foldl (.initAnd "broadcast") rval.nodes }
    | .flip nm out => rval := { rval with nodes := out.foldl (.initAnd nm) rval.nodes   }
    | .and nm out => rval := {rval with nodes := out.foldl (.initAnd nm) rval.nodes }

  pure rval

def send (q : Signals) (nm : String) (v : Bool) (out : List String) :=
  out.foldl (λ q n => q.enqueue (nm, n, v)) q

def Machine.update (m : Machine) (nm : String) (node : Node) :=
  { m with nodes := m.nodes.insert nm node }

partial
def Machine.run (m : Machine) (q : Signals) : Machine :=
  match q.dequeue? with
  | .none => m
  | .some ((fr, nm, val), q) =>
  let m := if val then { m with high := m.high + 1 }
    else { m with low := m.low + 1 }

  -- dbg_trace "{m.epoch}: {fr} sends {val} to {nm} "
  if fr == m.probe && val
  then { m with halt := true}
  else
  if nm == "broadcaster"
  then let q := send q nm val m.broadcast; m.run q
  else match m.nodes.find? nm with
  | .none => let m := ite val m {m with halt := true}
             m.run q
  | .some (.flip st out) => if val then m.run q else
         let m := m.update nm (.flip (!st) out)
         let q := send q nm (!st) out
         m.run q
  | .some (.nand mem out) =>
      let mem := mem.insert fr val
      let m := m.update nm (.nand mem out)
      let st := mem.toList.any λ v => !v.2
      let q := send q nm st out
      m.run q

def part1 (m : Machine) : IO Unit := do
  let mut out := m
  for _ in [0:1000] do
    out := out.run (send default "" false ["broadcaster"])
  println! "{out.low} {out.high} {out.low*out.high}"

partial
def probe (m : Machine) (n : Nat) (lab : String): Nat :=
  let m := {m with epoch := m.epoch.succ, probe := lab }
  let m := m.run (send default "" false ["broadcaster"])
  if m.halt then n else
  if n > 10000 then 0 else
  match m.nodes.find? lab with
  | .some (.nand mem _) =>
        if mem.toList.all λ (_,v) => !v then n
        else probe m n.succ lab
  | _ =>  dbg_trace "bad {lab}"; 0

abbrev NodeList := RBMap String Unit compare
def lcm (n : Nat) (m : Nat) := n * m / (n.gcd m)


def part2 (m : Machine) : IO Unit := do
  println! "part2"
  -- these feed into the "vt" and gate which feeds into rx
  let periods := ["bt", "fv", "rd", "pr"].map λ lab => probe m 1 lab
  let result := periods.foldl lcm 1
  println! "{result}"

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let machine <- parse input
  println! fname
  part1 machine
  if fname.endsWith "input.txt" then
    part2 machine


-- #eval main ["day20/eg.txt"]
#eval main ["day20/eg2.txt"]
#eval main ["day20/input.txt"]

-- and is tricky, we need a list of inputs...
