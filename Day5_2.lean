structure Range where
  lower: Nat
  upper: Nat
  h: lower <= upper
deriving Repr

instance: Ord Range where compare a b := compare a.lower b.lower
instance: LE Range := leOfOrd
instance: LT Range := ltOfOrd

@[grind =]
theorem Range.le_lower (a b: Range) :
    (a <= b) = (a.lower <= b.lower) := by 
  unfold LE.le instLERange leOfOrd instLENat compare instOrdRange compare instOrdNat compareOfLessAndEq Ordering.isLE
  grind only [cases Or]

@[grind =]
theorem Range.lt_lower (a b: Range) :
    (a < b) = (a.lower < b.lower) := by 
  unfold LT.lt instLTRange ltOfOrd instLTNat compare instOrdRange compare instOrdNat compareOfLessAndEq
  simp
  grind only [cases Or]

@[grind =]
theorem Range.le_lt (a b: Range) : (a < b) = ¬(b <= a) := by grind

instance: Inhabited Range where default := ⟨0, 0, by omega⟩
def Range.size (r: Range) := r.upper - r.lower + 1

structure RangeSet where
  ranges: List Range
  h: ranges.Pairwise fun a b => a.upper < b.lower

instance: Inhabited RangeSet where default := ⟨[], List.Pairwise.nil⟩
instance: Membership Range RangeSet where
  mem rs := Membership.mem rs.ranges

abbrev Sorted [LE α] (xs: List α): Prop := xs.Pairwise (· <= ·)

def RangeSet.ofRanges (ranges: List Range): RangeSet :=
  let rs := ranges.mergeSort
  have h: Sorted rs := by grind [List.sorted_mergeSort]

  let rec build (done: RangeSet) (current: Range) (rest: List Range) 
      (h_rest: Sorted rest)
      (h1: (r: Range) -> r ∈ done.ranges -> r.upper < current.lower)
      (h2: (r: Range) -> r ∈ rest -> current <= r): RangeSet :=
    let rs := RangeSet.mk (done.ranges ++ [current]) <| by
      simp [List.pairwise_append, done.h]
      grind only [→ List.Pairwise.of_cons]
    match rest with
    | [] => rs
    | x::xs => 
      if h: current.upper >= x.lower then
        let nc: Range := ⟨
          current.lower, max current.upper x.upper,
          by have := current.h; omega
        ⟩
        build done nc xs 
          (h_rest := by grind only [!List.Pairwise.of_cons])
          (h1 := by grind)
          (h2 := by grind)
      else
        build rs x xs
          (h_rest := by grind only [!List.Pairwise.of_cons])
          (h1 := by have := current.h; grind)
          (h2 := by grind [List.pairwise_cons])

  match rs with
  | [] => default
  | x::xs => build default x xs 
    (h_rest := by grind only [→ List.Pairwise.of_cons])
    (h1 := fun r h => absurd h List.not_mem_nil)
    (h2 := by grind [List.pairwise_cons])

def parseRange (s: String): Range :=
  let parts := s.splitOn "-" |>.map String.toNat!
  let a := parts[0]!
  let b := parts[1]!
  if h: a <= b then ⟨parts[0]!, parts[1]!, h⟩ else panic! "bad range"

def main : IO Unit := do
  let contents <- IO.FS.readFile "input5.txt"
  let parts := contents.splitOn "\n\n"
  let ranges := parts[0]! |>.trim.splitOn "\n" |>.map parseRange
  let rangeset := RangeSet.ofRanges ranges
  let answer := rangeset.ranges.map Range.size |>.sum
  IO.println answer
