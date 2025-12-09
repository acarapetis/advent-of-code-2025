def Matrix (T: Type) (height: Nat) (width: Nat) := Vector (Vector T width) height
structure Grid  (T: Type) where
  height: Nat
  width: Nat
  data: Matrix T height width
instance: Inhabited (Grid T) where default := ⟨0, 0, #v[]⟩

def Grid.ofRows [Inhabited T] (rows: Array (Array T)): Grid T :=
  if let some first := rows[0]? then
    ⟨rows.size, first.size, Vector.ofFn fun i => Vector.ofFn fun j => rows[i]![j]!⟩
  else default

def transpose (x: Matrix T a b): Matrix T b a :=
  Vector.ofFn fun i => Vector.ofFn fun j => x.get j |>.get i

def spaceSplit (s: String): List String := s.splitOn " " |>.filter (· != "")

inductive Op where
| Mult : Op
| Add : Op
deriving Inhabited

def parseOp: String -> Op
| "*" => Op.Mult
| "+" => Op.Add
| _ => panic! "bad op string"

def applyOp: Op -> List Nat -> Nat
| Op.Mult, d => d.foldl (· * ·) 1
| Op.Add, d => d.foldl (· + ·) 0

def blank (d: List Char) := d.all (· == ' ')
def sploot [Inhabited T] (p: T -> Bool) (xs: List T) :=
  xs.splitBy (p · == p ·) |>.filter fun xs => ! xs.all p

def parseCol (xs: List Char): Nat := 
  xs.filter (· != ' ') |>.reverse |> String.mk |>.toNat!

def main : IO Unit := do
  let contents <- IO.FS.readFile "input6.txt"
  let lines := contents.trim.splitOn "\n"
  if let opline::datalines := lines.reverse then
    let ops := spaceSplit opline |>.map parseOp
    let rows := datalines.map (·.data.toArray) |>.toArray
    let grid := Grid.ofRows rows |>.data |> transpose
    let cols := grid.toList.map (·.toList)
    let colgroups := sploot blank cols
    let numlists := colgroups.map (·.map parseCol)
    let results := ops.zipWith applyOp numlists
    IO.println <| results.sum
