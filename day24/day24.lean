import Lean

structure Point where
  x : Int
  y : Int
  z : Int
deriving Repr, BEq, Inhabited

structure Line where
  pos : Point
  vel : Point
deriving Repr, BEq, Inhabited

instance : HAdd Point Point Point where
  hAdd p₁ p₂ := ⟨ p₁.x + p₂.x, p₁.y + p₂.y, p₁.z + p₂.z ⟩

instance : HSub Point Point Point where
  hSub p₁ p₂ := ⟨ p₁.x - p₂.x, p₁.y - p₂.y, p₁.z - p₂.z ⟩

instance : HDiv Point Int Point where
  hDiv p n := ⟨ p.x / n, p.y / n, p.z / n ⟩

instance : HDiv Point Point Point where
  hDiv p q := ⟨ p.x / q.x, p.y / q.y, p.z / q.z ⟩

instance : HMul Point Int Point where
  hMul p n := ⟨ p.x  * n, p.y * n, p.z * n ⟩

instance : Min Point where
  min a b := ⟨ min a.x b.x, min a.y b.y, min a.z b.z ⟩

instance : Max Point where
  max a b := ⟨ max a.x b.x, max a.y b.y, max a.z b.z ⟩

instance : HAdd Line Line Line where
  hAdd l1 l2 := ⟨ l1.pos + l2.pos, l1.vel + l2.vel ⟩

instance : HDiv Line Int Line where
  hDiv l n := ⟨ l.pos / n, l.vel / n ⟩

instance : Min Line where
  min a b := ⟨ min a.pos b.pos, min a.vel b.vel ⟩

instance : Max Line where
  max a b := ⟨ max a.pos b.pos, max a.vel b.vel ⟩

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


--def det (a b c d : Float) :=  a * d - b * c
def det (a b c d : Int) :=  -- a * d - b * c
  let a := a.toNat
  let b := b.toNat
  let c := c.toNat
  let d := d.toNat
  let x := a*d
  let y := b*c
  if x > y then Int.ofNat (x-y) else -1*Int.ofNat (y-x)

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


  -- let top_x := det
  --     (det x₁ y₁ x₂ y₂)
  --     (det x₁ 1 x₂ 1)
  --     (det x₃ y₃ x₄ y₄)
  --     (det x₃ 1 x₄ 1)

  -- let top_y := det
  --     (det x₁ y₁ x₂ y₂)
  --     (det y₁ 1 y₂ 1)
  --     (det x₃ y₃ x₄ y₄)
  --     (det y₃ 1 y₄ 1)

  -- let bot := det
  --     (det x₁ 1 x₂ 1)
  --     (det y₁ 1 y₂ 1)
  --     (det x₃ 1 x₄ 1)
  --     (det y₃ 1 y₄ 1)

--  Find bad point (samey issue) and investigate.  it's float subtract.. make isect check answers
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
          let inside :=  min <= x && x <= max && min <= y && y <= max
          if inside then
            total := total + 1
  println! "part1 {total / 2}"

def mkRow (a b : Line) : Array Int :=
  #[
    b.vel.y - a.vel.y, -- x
    a.vel.x - b.vel.x, -- y
    a.pos.y - b.pos.y, -- vx
    b.pos.x - a.pos.x, -- vy
    a.pos.y*a.vel.x - b.pos.y*b.vel.x + b.pos.x*b.vel.y - a.pos.x*a.vel.y
  ]

abbrev Row := Array Int
abbrev Matrix := Array Row
instance : HAdd Row Row Row where
  hAdd a b := a.mapIdx λ ix v => v + b.get! ix

instance : HMul Row Int Row where
  hMul a n := a.map (·*n)

instance : HDiv Row Int Row where
  hDiv a n := a.map (·/n)

instance : ToString Matrix where
  toString mtx :=
    mtx.foldl (λ a row => a ++ " ".intercalate (row.toList.map toString) ++ "\n") ""

def Matrix.elim (mtx : Matrix) (col row : Nat) : Matrix :=
  let src := mtx.get! col
  let tgt := mtx.get! row
  let a := src.get! col
  let b := tgt.get! col
  if b == 0 then mtx else
  let g := a.natAbs.gcd b.natAbs
  -- dbg_trace "{src} {tgt} ai:{a/Int.ofNat g} bi:{b/Int.ofNat g}"
  let src := src * (b / Int.ofNat g)
  let dst := tgt * (a / Int.ofNat g)
  let src := if src[col]! * dst[col]! > 0 then src * -1 else src
  -- dbg_trace "foo {a} {b} {a*b} {src} {dst}"
  mtx.set! row (src + dst)

def gaussian (mtx : Matrix) : IO Matrix := do
  -- yay
  let mut mtx := mtx
  -- get non-zero in the right columns

  for i in [0:mtx.size] do
    if mtx[i]![i]! == 0 then
      let .some ix := mtx.findIdx? (λ x => x[i]! != 0)
        | dbg_trace "FAIL"; return mtx
      mtx := mtx.set! i (mtx[i]! + mtx[ix]!)

  for col in [0:mtx.size] do
    for row in [0:mtx.size] do
      if row != col then
        mtx := mtx.elim col row

  for row in [0:mtx.size] do
    let r := mtx.get! row
    let r := r / r[row]!
    mtx := mtx.set! row r
  pure mtx

def part2 (lines : List Line) : IO Unit := do
  let a :: b :: c :: d :: e :: _ := lines | println! "need five lines"
  let mtx := #[
    mkRow a b,
    mkRow a c,
    mkRow a d,
    mkRow a e
  ]

  let result <- gaussian mtx
  let #[x,y,vx,vy] := result.map (·.get! mtx.size) | pure ()

  let ta := (a.pos.x - x) / (vx - a.vel.x)
  let za := (a.pos.z + ta*a.vel.z)
  let tb := (b.pos.x - x) / (vx - b.vel.x)
  let zb := (b.pos.z + tb*b.vel.z)
  let vz := (zb - za)/(tb - ta)
  let z := za - vz*ta
  println! "{x} {y} {z} @ vx {vx} vy {vy} vz {vz}"
  let result := x + y + z
  println! "part2 {result}"

def main(args : List String) : IO Unit := do
  let fname :: min :: max :: _ := args | println! "too few arguments"
  let input <- IO.FS.readFile fname
  let .some lines := parseFile input | println! "parse error"
  let .some min := Float.ofInt <$> min.toInt? | println! "{min} not float"
  let .some max := Float.ofInt <$> max.toInt? | println! "{max} not float"

  println! fname
  part1 lines min max
  part2 lines

#eval main ["day24/eg.txt", "7", "27"]
#eval main ["day24/input.txt","200000000000000","400000000000000"]
