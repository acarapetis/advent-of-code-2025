abbrev Turn := Fin 100

def parseTurn (s : String) : Turn :=
  if s.isEmpty then
    panic! "Bad input"
  else
    match s.front with
      | 'L' => -(Fin.ofNat 100 (String.toNat! (s.drop 1)))
      | 'R' => (Fin.ofNat 100 (String.toNat! (s.drop 1)))
      | _ => panic! "Bad input"

-- #eval parseTurn "R50" + parseTurn "L68" + parseTurn "L30" + parseTurn "R48"

def evolveState: Turn -> List Turn -> List Turn
| x, [] => [x]
| x, y::ys => (x)::(evolveState (x+y) ys)

-- #eval evolveState 50 [-68, -30, 48]

def main (_args : List String) : IO Unit := do
  let contents ← IO.FS.readFile "input1.txt"
  let lines := List.filter (fun s => String.length s > 0) (contents.splitOn "\n")
  let turns := List.map parseTurn lines
  let states := evolveState 50 turns
  let password := List.countP (fun x => x== 0) states
  IO.println password
