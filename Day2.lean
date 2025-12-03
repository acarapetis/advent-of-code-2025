def parseRange (s: String): String × String :=
  let parts := s.splitOn "-"
  (parts[0]!, parts[1]!)

def split (s: String): Option (Nat × Nat) :=
  let l := s.length
  if l % 2 == 0 
  then some (s.take (l/2) |>.toNat!, s.drop (l/2) |>.toNat!)
  else none
def nextSeed (s: String): Nat :=
  match split s with
  | none => 10^((s.length - 1)/2) -- of the form 10001000
  | some (a,b) => if b <= a then a else a+1

def prevSeed (s: String): Nat :=
  match split s with
  | none => 10^((s.length - 1)/2) - 1 -- of the form 999999
  | some (a,b) => if b >= a then a else a-1

def sumRange (a: Nat) (b: Nat) := (b*(b+1) - a*(a-1))/2

def numDigits (n: Nat) := toString n |>.length
def seedToId (n: Nat) :=
  let s := toString n
  s.append s |>.toNat!
  
-- This is much slower than it could be - we don't actually need to enumerate all the
-- invalid IDs, we could instead use the summation formula sumRange above. Unfortunately
-- it's possible that the starting and ending points of the range could have different
-- digit counts, so we'd have to break up our range into subranges by digit count and I
-- don't have the mental energy right now so here's the dumb way, still runs plenty fast
-- on the input we're given
def sumInvalidIdsInRange : String × String -> Nat
| (a, b) =>
  let x := nextSeed a
  let y := prevSeed b
  if x > y then 0 else
    List.range' x (y-x+1) |>.map seedToId |>.sum


def main : IO Unit := do
  let contents <- IO.FS.readFile "input2.txt"
  let ranges := contents.stripSuffix "\n" |>.splitOn "," |> List.map parseRange
  IO.println ranges
  let sums := ranges |>.map sumInvalidIdsInRange
  IO.println sums
  let answer := sums.sum
  IO.println answer
