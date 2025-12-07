structure Range where
  lower: Nat
  upper: Nat
  h: lower <= upper
deriving Repr, Ord

instance: LE Range where le a b := (compare a b).isLE
instance: Inhabited Range where default := ⟨0, 0, by omega⟩
def Range.size (r: Range) := r.upper - r.lower + 1

structure RangeSet where
  ranges: List Range
  h: ranges.Pairwise fun a b => a.upper < b.lower

instance: Inhabited RangeSet where default := ⟨[], List.Pairwise.nil⟩

def RangeSet.ofRanges (ranges: List Range): RangeSet :=
  let rs := ranges.mergeSort (· <= ·)
  match rs with
  | [] => default
  | first::rest => Id.run do
      let mut out: List Range := []
      let mut a := first
      for b in rest do
        if a.upper >= b.lower then 
          a := ⟨a.lower, max a.upper b.upper, by sorry⟩
        else
          out := out.append [a]
          a := b
      out := out.append [a]
      ⟨out, by sorry⟩

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

