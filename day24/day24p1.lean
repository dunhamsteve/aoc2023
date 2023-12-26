import Lean

structure Point where
  x : Int
  y : Int
  z : Int
deriving Repr, BEq

structure Line where
  pos : Point
  vel : Point
deriving Repr, BEq

instance : ToString Point where toString p := s!"({p.x}, {p.y}, {p.z})"

instance : ToString Line where toString l := s!"{l.pos} @ {l.vel}"

def parseLine (line : String) : Option Line := do
  let [p, v] := line.splitOn " @ " | .none
  let .some [x,y,z] := p.splitOn ", " |>.mapM (·.trim.toInt?)
      | .none
  let .some [vx,vy,vz] := v.splitOn ", " |>.mapM (·.trim.toInt?)
      | .none
  .some ⟨ ⟨ x, y, z⟩ , ⟨ vx, vy, vz ⟩ ⟩

#eval ("19, 13, 30 @ -2,  1, -2".splitOn " @ ").map λ x => (x.splitOn ", ").map (·.trim.toInt?)
#eval parseLine "19, 13, 30 @ -2,  1, -2"

def parseFile (content : String) : Option (List Line) :=
  let lines := content.trim.splitOn "\n"
  lines.mapM parseLine

-- ## Math

def isect (a b : Line) : Option (Float × Float × Float × Float) :=
  let ⟨ x₁, y₁, _ ⟩ := a.pos

  let x₂ := x₁ + a.vel.x
  let y₂ := y₁ + a.vel.y

  let ⟨ x₃, y₃, _ ⟩  := b.pos

  let x₄ := x₃ + b.vel.x
  let y₄ := y₃ + b.vel.y

  let top_x := (x₁*y₂ - y₁*x₂)*(x₃-x₄) + (x₁-x₂)*(y₃*x₄-x₃*y₄)
  let top_y := (x₁*y₂ - y₁*x₂)*(y₃-y₄) + (y₁-y₂)*(y₃*x₄-x₃*y₄)
  let bot := (x₁-x₂)*(y₃-y₄) - (y₁-y₂)*(x₃-x₄)

  let top_x := Float.ofInt top_x
  let top_y := Float.ofInt top_y
  let bot := Float.ofInt bot

  if bot == 0 then .none
  else
    let xᵢ := top_x/bot
    let yᵢ := top_y/bot
    if a.vel.x == 0 then dbg_trace "ZARO"; .none
    else
    if b.vel.x == 0 then dbg_trace "ZARO"; .none
    else
    let t₁ := (xᵢ - Float.ofInt x₁)/(.ofInt a.vel.x)
    let t₂ := (xᵢ - Float.ofInt x₃)/(.ofInt b.vel.x)
   .some (xᵢ, yᵢ, t₁, t₂)

def foo := "72561138243744, 176786320665271, 300472253244764 @ 84, 140, 55"
def bar := "417745975923774, 293484304091146, 309738404744489 @ -119, -7, 33"

#eval isect <$> parseLine foo <*> parseLine bar
#eval isect <$> parseLine bar <*> parseLine foo

#eval isect <$> parseLine "19, 13, 30 @ -2, 1, -2" <*> parseLine "18, 19, 22 @ -1, -1, -2"
#eval isect <$> parseLine "18, 19, 22 @ -1, -1, -2" <*> parseLine "20, 25, 34 @ -2, -2, -4"

def part1 (lines : List Line) (min max : Float) : IO Unit := do
  let mut total := 0
  for l in lines do
    for k in lines do
      if l == k then continue
      match isect l k with
      | .none => pure () -- println! "{l} {k} are parallel"
      | .some (x,y,t₁,t₂) => do
          if t₁ < 0 || t₂ < 0 then continue
          if (x-min).abs < 1 || (x-max).abs < 1 || (y-min).abs < 1 || (y-max).abs < 1 then
            println! "close {x-min} {x-max} {y-min} {y-max}"
          if (t₁).abs <1 || t₂.abs < 1 then
            println! "close2 {t₁} {t₂}"
          let inside :=  min <= x && x <= max && min <= y && y <= max
          if inside then
            total := total + 1
  println! total / 2

def main(args : List String) : IO Unit := do
  let fname :: min :: max :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .some lines := parseFile input | println! "parse error"
  let .some min := Float.ofInt <$> min.toInt? | println! "{min} not float"
  let .some max := Float.ofInt <$> max.toInt? | println! "{max} not float"

  println! fname
  part1 lines min max

#eval main ["day24/eg.txt", "7", "27"]
#eval main ["day24/input.txt","200000000000000","400000000000000"]
