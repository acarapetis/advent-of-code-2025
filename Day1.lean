abbrev Turn := Fin 100

def parseTurn (s : String) : Turn :=
  let count := s.drop 1 |> String.toNat! |> Fin.ofNat 100
  match s.front with
    | 'L' => -count
    | 'R' => count
    | _ => panic! "Bad input"

-- #eval parseTurn "R50" + parseTurn "L68" + parseTurn "L30" + parseTurn "R48"

def evolveState: Turn -> List Turn -> List Turn
| x, [] => [x]
| x, y::ys => x::(evolveState (x+y) ys)

-- #eval evolveState 50 [-68, -30, 48]

def main : IO Unit := do
  let contents ← IO.FS.readFile "input1.txt"
  let lines := List.filter (·.length > 0) (contents.splitOn "\n")
  let turns := List.map parseTurn lines
  let states := evolveState 50 turns
  let password := List.countP (· == 0) states
  IO.println password
