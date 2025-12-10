abbrev Joltages (n: Nat) := Vector Nat n
abbrev Button (n: Nat) := {b : Vector Bool n // true ∈ b}

structure Problem where
  n: Nat
  targetJoltages: Joltages n
  buttons: List (Button n)
deriving Inhabited, Repr

def parseButton (n: Nat) [hn: NeZero n] (s: String): Option (Button n) :=
  let nums := s.drop 1 |>.dropRight 1 |>.splitOn "," |>.map String.toNat!
  if h: nums ≠ [] then
    let n0 := nums.head h
    let vec := nums.foldl (init := Vector.replicate n false) fun v i => v.set! i true
    some ⟨vec, by sorry
      /- have : n0 < n := by sorry -/
      /- have : vec[n0] := by grind -/
      /- grind -/
    ⟩
  else none

def parseLine (s: String): Problem :=
  let chunks := s.trim.splitOn " " |>.drop 1
  if h: chunks ≠ [] then
    let jStr := chunks.getLast h
    let xs := chunks.dropLast
    let joltages := jStr.drop 1 |>.dropRight 1 
        |>.splitOn "," |>.map String.toNat! |>.toArray |>.toVector
    let n := joltages.size
    if h : n ≠ 0 then
      have : NeZero n := ⟨h⟩
      ⟨n, joltages, xs.filterMap (parseButton n)⟩
    else panic! s!"Bad input {s}"
  else panic! s!"Bad input {s}"

def sub  (j: Joltages n) (b: Button n): Option (Joltages n) := 
  let opts := j.zipWith (bs := b) fun (x: Nat) (y: Bool) => 
    if y then
      if x = 0 then none
      else some <| x - 1
    else some x
  opts.mapM id

def opsToZero (p: Problem) (j: Joltages p.n): Option Nat :=
  if j.all (· == 0) then 
    some 0 
  else
    let f (b : Button p.n): Option Nat := do
      let new <- sub j b
      opsToZero p new
    p.buttons.filterMap f |>.min? |>.map (· + 1)
termination_by j.sum
decreasing_by sorry

def solve (p: Problem): Nat := opsToZero p p.targetJoltages |>.get!

--def p1 := parseLine "[..] (0) (1) (0,1) (2) {1,2,3}"
def p1 := parseLine "[.#.] (1) (2) (2) (0,2) (0,1) {3,5,4}"
def j1 := p1.targetJoltages
#eval! opsToZero p1 j1


def main: IO Unit := do
  let content <- IO.FS.readFile "input10small.txt"
  let problems := content.trim.splitOn "\n" |>.map parseLine
  problems.map solve |>.sum |> IO.println
