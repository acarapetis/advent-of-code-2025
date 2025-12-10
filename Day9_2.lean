abbrev Point := Int × Int
inductive Edge where
| Vertical (x: Int) (y1: Int) (y2: Int)
| Horizontal (x1: Int) (x2: Int) (y: Int)
deriving Inhabited, Repr
abbrev Polygon := List Edge

def edge (p: Point) (q: Point): Edge :=
  if p.fst = q.fst then
    Edge.Vertical p.fst p.snd q.snd
  else if p.snd = q.snd then
    Edge.Horizontal p.fst q.fst p.snd
  else
    panic! "Diagonal edge!?"

def parsePoint (s: String): Point :=
  let ns := s.trim.splitOn "," |>.map (·.toInt!)
  (ns[0]!, ns[1]!)

def opairs: List T -> List (T × T)
| [] => []
| x::xs => xs.map (x, ·) ++ opairs xs

def area: Point × Point -> Nat
| ((x1, y1), (x2, y2)) => ((x2-x1).natAbs + 1) * ((y2-y1).natAbs + 1)

def polygon (points: List Point) (h: points ≠ []): Polygon :=
  points.zip (points.tail ++ [points.head h]) |>.map fun (p, q) => edge p q

def rectangle (p: Point) (q: Point): Polygon :=
  polygon [p, (p.fst, q.snd), q, (q.fst, p.snd)] (by grind)

def Int.isBetween (self a b: Int) := (a < self && self < b) || (b < self && self < b)

  (x.isBetween u1 u2 && v.weakBetween y1 y2) ||
  (x.weakBetween u1 u2 && v.isBetween y1 y2)

def edgesIntersect: Edge -> Edge -> Bool
| Edge.Horizontal .., Edge.Horizontal .. => false
| Edge.Vertical .., Edge.Vertical .. => false
| Edge.Vertical x y1 y2, Edge.Horizontal u1 u2 v => _intersect x y1 y2 u1 u2 v
| Edge.Horizontal u1 u2 v, Edge.Vertical x y1 y2 => _intersect x y1 y2 u1 u2 v

def cuts (a: Polygon) (b: Polygon): Bool :=
  a.any fun x => b.any fun y => edgesIntersect x y

def main: IO Unit :=  do
  let content <- IO.FS.readFile "input9small.txt" 
  let points := content.trim.splitOn "\n" |>.map parsePoint
  if h: points ≠ [] then
    let shape := polygon points h
    let best := opairs points
      |>.map (fun pair => (pair, area pair))
      |>.mergeSort (fun (_, a1) (_, a2) => a1 <= a2)
      |>.reverse
      |>.find? (fun ((p, q), _) => !cuts (rectangle p q) shape)
    if let some x := best then
      IO.println x
