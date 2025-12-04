structure Grid where
  height: Nat
  width: Nat
  [hHeight: NeZero height]
  [hWidth: NeZero width]
  data: Fin height × Fin width -> Bool

instance: Inhabited Grid where
  default := ⟨1, 1, fun _ => False⟩

abbrev Grid.coord (g: Grid) := Fin g.height × Fin g.width
def Grid.cells (g: Grid): List g.coord :=
  List.finRange g.height |>.flatMap fun r =>
    List.finRange g.width |>.map fun c => (r,c)

def parseGrid (s: String): Grid := Id.run do
  let lines := s.stripSuffix "\n" |>.splitOn "\n"
  let height := lines.length
  if h: height = 0 then panic! "Empty grid!" else
    have : NeZero height := ⟨h⟩
    let width := lines[0].length
    if hw: width = 0 then panic! "Empty grid!" else
      have : NeZero width := ⟨hw⟩
      let data (p: Fin height × Fin width) := lines[p.fst].data[p.snd]! == '@'
      ⟨height, width, data⟩ 

def product (l1 : List α) (l2 : List β) : List (α × β) :=
  l1.flatMap fun x => l2.map fun y => (x, y)

def Grid.neighbours (g: Grid) (p: g.coord): List g.coord :=
  have : NeZero g.width := g.hWidth
  have : NeZero g.height := g.hHeight
  let r := p.fst.val
  let c := p.snd.val
  let rows := [r-1, r, r+1].eraseDups.filter (· < g.height)
  let cols := [c-1, c, c+1].eraseDups.filter (· < g.width)
  let coords := product rows cols |>.filter fun (x, y) => x != r || y != c
  coords.map fun (x,y) => (Fin.ofNat g.height x, Fin.ofNat g.width y)

def Grid.accessible (g: Grid) (p: g.coord) := Id.run do
  if !(g.data p) then return false
  return (g.neighbours p |>.filter g.data |>.length) < 4

def main : IO Unit := do
  let contents <- IO.FS.readFile "input4.txt"
  let grid := parseGrid contents
  
  IO.println <| grid.cells.countP grid.accessible
