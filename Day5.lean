structure Range where
  lower: Nat
  upper: Nat
  
instance : ToString Range where
  toString r := s!"{r.lower}-{r.upper}"

def Range.includes (r: Range) (id: Nat) := r.lower <= id && id <= r.upper

def parseRange (s: String): Range :=
  let parts := s.splitOn "-"
  ⟨parts[0]!.toNat!, parts[1]!.toNat!⟩

def main : IO Unit := do
  let contents <- IO.FS.readFile "input5.txt"
  let parts := contents.splitOn "\n\n"
  let ranges := parts[0]! |>.trim.splitOn "\n" |>.map parseRange
  let ids := parts[1]! |>.trim.splitOn "\n" |>.map String.toNat!
  let answer := ids.countP fun id => ranges.any fun r => r.includes id
  IO.println answer
