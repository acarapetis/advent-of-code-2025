abbrev Lights (n: Nat) := BitVec n
abbrev Button (n: Nat) := BitVec n

def BitVec.popcount (bv: BitVec n): Nat :=
  List.finRange n |>.map (bv[·].toNat) |>.sum

structure Problem where
  n: Nat
  targetLights: Lights n
  buttons: List (Button n)
deriving Inhabited

def parseButton (n: Nat) [NeZero n] (s: String): Button n :=
  s.drop 1 |>.dropRight 1 |>.splitOn "," |>.map String.toNat! 
      |>.foldl (init := 0#n) (fun bv m =>
    if m < n then bv ^^^ (1#n <<< m) else panic! "Bad index in button"
  )

def parseLine (s: String): Problem :=
  let chunks := s.trim.splitOn " "
  if let x::xs := chunks.dropLast then
    let ls := x.drop 1 |>.dropRight 1
    let bools := ls.data.map (· == '#')
    let n := bools.length
    if h : n ≠ 0 then
      have : NeZero n := ⟨h⟩
      let lights := BitVec.ofBoolListLE bools
      ⟨n, lights, xs.map (parseButton n)⟩
    else panic! s!"Bad input {s}"
  else panic! s!"Bad input {s}"

def minPresses (p: Problem): Nat :=
  let m := p.buttons.length
  let seqs := List.range (2^m) |>.map (BitVec.ofNat m)
  let apply (seq: BitVec m) := List.range m |>.foldl (init := 0#p.n) fun bv i => 
    if seq[i]! then bv ^^^ p.buttons[i]! else bv
  let results := seqs.filter (apply · == p.targetLights)
  results.map BitVec.popcount |>.min?.get!

def main: IO Unit := do
  let content <- IO.FS.readFile "input10.txt"
  let problems := content.trim.splitOn "\n" |>.map parseLine
  problems.map minPresses |>.sum |> IO.println
