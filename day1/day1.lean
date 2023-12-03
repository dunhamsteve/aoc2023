import Lean

def digits1 (text : String) :=
  (text.toList.filter (λ c => c >= '0' && c <= '9')).map (·.toNat - 48)

def replace (text : String) :=
  text.replace "one"  "1"

def digits2 (text : String) :=
  let rec go : List Char -> List Nat -> List Nat
    | 'o' :: x@('n' :: 'e' ::  _), acc               => go x (1 :: acc)
    | 't' :: x@('w' :: 'o' ::  _), acc               => go x (2 :: acc)
    | 't' :: x@('h' :: 'r' :: 'e' :: 'e' ::  _), acc => go x (3 :: acc)
    | 'f' :: x@('o' :: 'u' :: 'r' ::  _), acc        => go x (4 :: acc)
    | 'f' :: x@('i' :: 'v' :: 'e' ::  _), acc        => go x (5 :: acc)
    | 's' :: x@('i' :: 'x' ::  _), acc               => go x (6 :: acc)
    | 's' :: x@('e' :: 'v' :: 'e' :: 'n' ::  _), acc => go x (7 :: acc)
    | 'e' :: x@('i' :: 'g' :: 'h' :: 't' ::  _), acc => go x (8 :: acc)
    | 'n' :: x@('i' :: 'n' :: 'e' ::  _), acc        => go x (9 :: acc)
    | (c :: cs), acc => if c.isDigit
      then go cs ((c.toNat - 48) :: acc)
      else go cs acc
    | [], acc => acc.reverse
  go text.toList []

def part1 (text : String) (digits : String -> List Nat):=
  let lines := text.trim.splitOn "\n";
  -- The head! / getLast! substitute the default value
  let nums := (lines.map digits).map λ ds => ds.head! * 10 + ds.getLast!;
  nums.foldl (· + · ) 0

def main(args : List String) : IO Unit := do
  let fname :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let p1 := part1 input digits1
  let p2 := part1 input digits2
  println! fname
  println! "{fname}: {p1} {p2}"

#eval main ["day1/eg.txt"]
#eval main ["day1/eg2.txt"]
#eval main ["day1/input.txt"]
