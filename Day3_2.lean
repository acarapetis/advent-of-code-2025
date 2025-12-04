abbrev Bank := List Nat
def parseBank (s: String): Bank :=
  s.data.map (·.toNat - '0'.toNat)

@[simp] theorem maxExists [Max T] (b: List T) : b.max?.isSome = (b.length >= 1) := (by
  simp only [Option.isSome_iff_ne_none]
  simp only [ne_eq]
  simp only [List.max?_eq_none_iff]
  simp only [Nat.one_le_iff_ne_zero]
  simp only [ne_eq]
  simp only [List.length_eq_zero_iff]
)

abbrev CellCount := {n: Nat // n ≠ 0}

def maxJoltage (n: CellCount) (b: Bank): Nat :=
  if hb: b.length < n then panic! s!"Bank {b} too small for {n} cells" else
    match h: n.val with
    | 0 => absurd h n.property
    | 1 => b.max?.get (by simp; grind)
    | m+2 =>
      let n2: CellCount := ⟨m + 1, by omega⟩
      let candidates := b.take (b.length - n2)
      let digit := candidates.max?.get (by simp; grind)
      let pos := candidates.findIdx (· == digit)
      let remainder := b.drop <| pos + 1
      digit * 10^(n.val-1) + maxJoltage n2 remainder
termination_by n.val
decreasing_by omega

def main : IO Unit := do
  let contents <- IO.FS.readFile "input3.txt"
  let banks := contents.stripSuffix "\n" |>.splitOn "\n" |> List.map parseBank
  let joltages := banks.map (maxJoltage ⟨12, by omega⟩)
  let answer := joltages.sum
  IO.println answer
