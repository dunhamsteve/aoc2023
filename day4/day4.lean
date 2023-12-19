import Lean

abbrev Card := List Nat × List Nat

def parseCard (s : String) : Option Card := do
  let [_, b] := s.splitOn ": " | none
  let [l, r] := b.splitOn " | " | none
  let nums s := (s.splitOn.filter (·.length > 0)).mapA (·.trim.toNat?)
  (·,·) <$> nums l <*> nums r

def parse (content : String) : Option (List Card) :=
   let lines := content.trim.splitOn "\n"
   lines.mapM parseCard

def part1 (data : List Card) : IO Unit := do
  let mut total := 0
  for (a,b) in data do
    let n := (a.filter λ n => b.contains n).length
    if n > 0 then
      total := total +  ((2).pow n.pred)
  println! "part1 {total}"

def part2 (data : List Card) : IO Unit := do
  let data := data.toArray
  let l := data.size
  let mut counts : (x : Array Nat) ×' l = x.size  := ⟨ mkArray l 1, by simp ⟩
  for h : i in [0:l] do
    let (a,b) := data[i]'h.2
    let n := (a.filter λ n => b.contains n).length
    let mult := counts.1[i]'(by rw [<-counts.2]; exact h.2)
    for j in [0:n] do
      let ix := i + j + 1
      if h : ix < counts.1.size then
        counts := ⟨ counts.1.set ⟨ ix, h ⟩  (counts.1[ix] + mult), by simp [counts.2] ⟩

  let total := counts.1.foldl (·+·) 0
  println! "part2 {total}"

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let some data := parse input | println! "parse failed on {fname}"
  println! fname
  part1 data
  part2 data

#eval main ["day4/eg.txt"]
#eval main ["day4/input.txt"]
--
