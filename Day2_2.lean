def parseRange (s: String): String × String :=
  let parts := s.splitOn "-"
  (parts[0]!, parts[1]!)

def split (n: Nat) (s: String): Option (Fin n -> Nat) :=
  if s.length % n == 0 then
    let l := s.length / n
    some (fun i => s.drop (i.val * l) |>.take l |>.toNat!)
  else
    none

def asList [NeZero n] (v: Fin n -> T): List T :=
  List.range n |>.map (fun i => v (Fin.ofNat n i))

def pairs (xs: List T) := List.zip xs (List.drop 1 xs)
def greaterThanRepeato [NeZero n] (v: Fin n -> Int): Bool :=
  let z := v 0
  let rec good (l: List Int): Bool :=
    match l with
    | [] => false
    | x::xs => if x == z then good xs else x > z
  asList v |> good

def lessThanRepeato [NeZero n] (v: Fin n -> Int) := greaterThanRepeato (-v ·)

def nextSeed (n: Nat) [NeZero n] (s: String): Nat :=
  match split n s with
  | none => 10^(s.length/n) -- of the form 10001000
  | some l => if greaterThanRepeato (Int.ofNat ∘ l) then (l 0) + 1 else (l 0)

def prevSeed (n: Nat) [NeZero n] (s: String): Nat :=
  match split n s with
  | none => 10^(s.length/n) - 1 -- of the form 999999
  | some l => if lessThanRepeato (Int.ofNat ∘ l) then (l 0) - 1 else (l 0)

def rep: Nat -> String -> String
| 0, _ => ""
| n+1, s => s.append <| rep n s

def seedToId (n: Nat) [NeZero n] (seed: Nat) :=
  toString seed |> rep n |>.toNat!
  
def invalidIdsInRange: Nat -> String × String -> List Nat
| 0, _ => []
| n' + 1, range =>
  let n := n' + 1
  have : NeZero n := inferInstance
  let x := nextSeed n range.fst
  let y := prevSeed n range.snd
  if x > y then [] else
    List.range' x (y-x+1) |>.map (seedToId n)

def sumInvalidIdsInRange (range: String × String): Nat :=
  let ns := List.range' 2 (range.snd.length-1)
  let idLists := ns.map (invalidIdsInRange · range)
  idLists.flatten.eraseDups.sum

def main : IO Unit := do
  let contents <- IO.FS.readFile "input2.txt"
  let ranges := contents.stripSuffix "\n" |>.splitOn "," |>.map parseRange
  let sums := ranges.map sumInvalidIdsInRange
  IO.println sums.sum
