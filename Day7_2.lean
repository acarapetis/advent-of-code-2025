abbrev State (n: Nat) := Vector Nat n

def step (state: State n) (splitters: Vector Bool n): State n :=
  let splitFrom (i : Nat): Nat :=
    match splitters[i]?, state[i]? with
    | some true, some k => k
    | _, _              => 0

  state.zipIdx.map fun (beam, i) =>
    if splitters[i]! then 0 else beam + splitFrom (i-1) + splitFrom (i+1)

def parseRow (n: Nat) (c: Char) (s: String): Vector Bool n := 
  let a := s.data.map (· == c) |>.toArray
  if h: a.size = n then ⟨a, h⟩ else panic! "unexpected width"

def main : IO Unit := do
  let contents <- IO.FS.readFile "input7.txt"
  let rows := contents.trim.splitOn "\n"
  if let r::rs := rows then
    let n := r.length
    let splitterses := rs.map (parseRow n '^')
    let mut state := r |> parseRow n 'S' |>.map Bool.toNat
    for splitters in splitterses do
      state := step state splitters
    IO.println state.sum
