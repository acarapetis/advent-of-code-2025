def Matrix (T: Type) (height: Nat) (width: Nat) := Vector (Vector T width) height
abbrev Matrix.coord (_: Matrix T h w) := Fin h × Fin w
def Matrix.sample (g: Matrix T height width) (p: g.coord): T :=
  g.get p.fst |>.get p.snd

def Matrix.cells (g: Matrix T height width): List g.coord :=
  List.finRange height |>.flatMap fun r =>
    List.finRange width |>.map fun c => (r,c)

def Matrix.ofFn2 (f: (Fin height) × (Fin width) → T): Matrix T height width :=
  Vector.ofFn fun x => Vector.ofFn fun y => f (x,y)

structure Grid where
  height: Nat
  width: Nat
  data: Matrix Bool height width
instance: Inhabited <| Grid where default := ⟨0, 0, #v[]⟩

abbrev Grid.occupied (g: Grid) := g.data.sample
def Grid.occupiedCount (g: Grid) := g.data.cells.countP g.occupied
abbrev Grid.coord (g: Grid) := g.data.coord

def parseRow (s: String): Array Bool :=
  s.data.map (· == '@') |>.toArray

def parseGrid (s: String): Grid :=
  let lines := s.stripSuffix "\n" |>.splitOn "\n"
  let rows := lines.map parseRow
  if let some first := rows[0]? then
    let width := first.size
    let coerceRow (xs: Array Bool): Vector Bool width :=
      if h: xs.size = width then ⟨xs, h⟩ else panic! "Ragged input!"
    let rowVec := rows.map coerceRow |>.toArray.toVector
    ⟨rowVec.size, width, rowVec⟩
  else default

def product (l1 : List α) (l2 : List β) : List (α × β) :=
  l1.flatMap fun x => l2.map fun y => (x, y)

def Grid.neighbours (g: Grid) (p: g.coord): List g.coord :=
  if h: g.width = 0 ∨ g.height = 0 then [] else
    have : NeZero g.width := ⟨by omega⟩
    have : NeZero g.height := ⟨by omega⟩
    let r := p.fst.val
    let c := p.snd.val
    let rows := [r-1, r, r+1].eraseDups.filter (· < g.height)
    let cols := [c-1, c, c+1].eraseDups.filter (· < g.width)
    let coords := product rows cols |>.filter fun (x, y) => x != r || y != c
    coords.map fun (x,y) => (Fin.ofNat g.height x, Fin.ofNat g.width y)

def Grid.accessible (g: Grid) (p: g.coord) :=
  g.data.sample p && (g.neighbours p |>.filter g.data.sample |>.length) < 4

def Grid.removeAccessible (g: Grid): Grid :=
  {g with data := Matrix.ofFn2 fun p => g.occupied p && !g.accessible p}

theorem Matrix.ofFn2_sample (f: Fin h × Fin w → T) (p: Fin h × Fin w):
    (Matrix.ofFn2 f).sample p = f p := by
  unfold Matrix.ofFn2 Matrix.sample
  simp [Vector.ofFn, Vector.get]

theorem removeAccessibleNonIncreasing (g: Grid):
    g.removeAccessible.occupiedCount <= g.occupiedCount := by
  have (p: g.coord): g.removeAccessible.occupied p = (g.occupied p && !g.accessible p) := by
      unfold Grid.removeAccessible Grid.occupied Grid.accessible
      grind only [Matrix.ofFn2_sample, usr List.length_filter_le, cases eager Prod, cases Or]
  have hImpl (x : g.data.coord): x ∈ g.data.cells → 
    g.removeAccessible.occupied x -> g.occupied x := by
      grind only [cases eager Prod]
  exact List.countP_mono_left hImpl

def finalGrid (g: Grid): Grid :=
  let g2 := g.removeAccessible
  if g.occupiedCount == g2.occupiedCount then g else finalGrid g2
termination_by g.occupiedCount
decreasing_by
  grind only [!removeAccessibleNonIncreasing]

def main : IO Unit := do
  let contents <- IO.FS.readFile "input4.txt"
  let grid := parseGrid contents
  let n1 := grid.occupiedCount
  let n2 := (finalGrid grid).occupiedCount
  IO.println <| n1 - n2
