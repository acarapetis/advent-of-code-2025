structure State (n: Nat) where
  beams: Vector Bool n
  splitCount: Nat
deriving Repr

def step (state: State n) (splitters: Vector Bool n): State n :=
  let splitFrom (i : Nat) :=
    match splitters[i]?, state.beams[i]? with
    | some s, some b => s && b
    | _, _           => false

  let huh := state.beams.zipIdx.map fun (beam, i) =>
    let s1 := splitFrom (i-1)
    let s2 := splitFrom (i+1)
    (if splitters[i]! then false else beam || s1 || s2, s1.toNat + s2.toNat)

  { beams := huh.map Prod.fst,
    splitCount := state.splitCount + (huh.map Prod.snd |>.sum)/2}

def parseRow (n: Nat) (c: Char) (s: String): Vector Bool n := 
  let a := s.data.map (· == c) |>.toArray
  if h: a.size = n then ⟨a, h⟩ else panic! "unexpected width"

def main : IO Unit := do
  let contents <- IO.FS.readFile "input7.txt"
  let rows := contents.trim.splitOn "\n"
  if let r::rs := rows then
    let n := r.length
    let beams := r |> parseRow n 'S'
    let splitterses := rs.map (parseRow n '^')
    let mut state := ⟨beams, 0⟩
    for splitters in splitterses do
      state := step state splitters
    IO.println state.splitCount
