abbrev Bank := List Nat
def parseBank (s: String): Bank :=
  s.data.map (·.toNat - '0'.toNat)

def maxJoltage (b: Bank): Nat :=
  let firstCandidates := b.dropLast
  if let some first := firstCandidates.max? then
    let firstIdx := firstCandidates.findIdx (· == first)
    let lastCandidates := b.drop (firstIdx + 1)
    if let some last := lastCandidates.max? then
      first*10 + last
    else panic! "Bank too small!"
  else panic! "Bank too small!"

def main : IO Unit := do
  let contents <- IO.FS.readFile "input3.txt"
  let banks := contents.stripSuffix "\n" |>.splitOn "\n" |> List.map parseBank
  let joltages := banks.map maxJoltage
  let answer := joltages.sum
  IO.println answer
