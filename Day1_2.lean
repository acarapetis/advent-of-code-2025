abbrev Turn := Int

def parseTurn (s : String) : Turn :=
  let count := s.drop 1 |> String.toNat!
  match s.front with
    | 'L' => -count
    | 'R' => count
    | _ => panic! "Bad input"

def evolveState: Turn -> List Turn -> List Turn
| x, [] => [x]
| x, y::ys => x::(evolveState (x+y) ys)

def pairs (xs: List T) := List.zip xs (List.drop 1 xs)

def zero (t: Turn): Bool := t % 100 == 0

def clicks: Turn × Turn -> Int
| (x, y) => 
  if zero x && !zero y then (x-y).natAbs / 100 else 
    if (!zero x) && zero y then ((Rat.ofInt (x-y).natAbs)/100).ceil else
      ((x/100)-(y/100)).natAbs

def countClicks (states: List Turn) :=
  pairs states |> List.map clicks |> List.sum

def main : IO Unit := do
  let contents ← IO.FS.readFile "input2.txt"
  let lines := List.filter (·.length > 0) (contents.splitOn "\n")
  let turns := List.map parseTurn lines
  let states := evolveState 50 turns
  let password := countClicks states
  IO.println password
