import Lean

open Lean RBMap

abbrev Edge := String × String
abbrev Edges := List Edge

def parseLine (line : String) :  Option (List (String × String)) := do
  let [a, b] := line.splitOn ": " | .none
  let nodes := b.splitOn " "
  nodes.map (a,·)

def parseFile (content : String) : Option (List (String × String)) := do
  content.trim.splitOn "\n" |>.mapM parseLine |>.map List.join

def contract (a b : String) : Edges -> Edges
  | [] => []
  | (x, y) :: rest =>
      if x == a && y == b || x == b && y == a then contract a b rest
      else if x == a || x == b then (a ++ b, y) :: contract a b rest
      else if y == a || y == b then (x, a ++ b) :: contract a b rest
      else (x,y) :: contract a b rest

partial
def split (s : String) : List String :=
  if s == "" then [] else
  let hd := s.take 3
  let tl := s.drop 3
  hd :: split tl

def unique (edges : Edges) :  Edges :=
  edges.toArray.map (λ (a,b) => ite (a > b) (b,a) (a,b))
      |>.qsort (λ (a,b) (c,d) => a == c && b < d || a < c)
      |>.toList.eraseReps

partial
def karger (start : List Edge) (fuel : Nat): IO Unit := do
  if fuel == 0 then
    println! "FAIL"
  else
  let mut edges := start
  while edges.length > 1 do
    let i <- IO.rand 0 edges.length.pred
    let (a,b) := edges[i]!
    edges := unique <| contract a b edges
    -- println! "sorted {edges}"

  -- println! "got {edges}"
  let (a,b) := edges[0]!
  let as := split a
  let bs := split b
  let mut count := 0
  for a in as do
    for b in bs do
      if start.any λ x => x == (a,b) || x == (b,a) then
        count := count + 1
        if count > 3 then break
    if count > 3 then break
  if count == 3 then
    let result := a.length/3 * b.length/3
    println! "{a.length} {b.length} {result}"
  else
    println! "{1001-fuel} cut > 3"
    karger start fuel.pred

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .some edges := parseFile input | println! "parse error"
  println! fname
  println! "{edges.length} edges"
  karger edges 1000

#eval main ["day25/eg.txt"]
-- #eval main ["day25/input.txt"]
