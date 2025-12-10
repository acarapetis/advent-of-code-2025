abbrev Point := Int × Int
def parsePoint (s: String): Point :=
    let ns := s.trim.splitOn "," |>.map (·.toInt!)
    (ns[0]!, ns[1]!)

def opairs: List T -> List (T × T)
| [] => []
| x::xs => xs.map (x, ·) ++ opairs xs

def area: Point × Point -> Nat
| ((x1, y1), (x2, y2)) => ((x2-x1).natAbs + 1) * ((y2-y1).natAbs + 1)

def main: IO Unit :=  do
  let content <- IO.FS.readFile "input9.txt" 
  let points := content.trim.splitOn "\n" |>.map parsePoint
  opairs points |>.map area |>.max?.get! |> IO.println
