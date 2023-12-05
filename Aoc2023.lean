namespace Aoc2023

structure Grid (α : Type) where
  width : Nat
  height : Nat
  data : Array α
  h1 : data.size = height * width

deriving Repr

def lemma { x y w h : Nat} : x < w -> y < h -> y * w + x < h * w := by
  intro xw yh
  cases h1 : h
  . rw [h1] at yh
    contradiction
  . rename_i n
    rw [Nat.succ_mul]
    have : n * w + x < n * w + w := by simp [Nat.add_lt_add_left,xw]
    apply Nat.lt_of_le_of_lt _ this
    apply Nat.add_le_add_right
    apply Nat.mul_le_mul_right
    apply Nat.le_of_lt_succ
    rw [<-h1]
    exact yh

namespace Grid
  -- get which returns the default when off grid
  def get! [Inhabited α] (g : Grid α) (x : Nat) (y : Nat) : α :=
    if h : x < g.width ∧ y < g.height then
      have h : y * g.width + x < g.data.size := by simp [g.h1, h, lemma]
      g.data[y*g.width + x]'h
    else
      default
end Grid

-- default char is 'A' ?
#eval (default : Char)

end Aoc2023
