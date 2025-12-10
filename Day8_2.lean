import Std.Data.HashMap
open Std (HashMap)

structure Pos where
  x : Int
  y : Int
  z : Int
  deriving Inhabited, Repr

def VertId := UInt16 deriving BEq, Hashable, OfNat, LE, Inhabited, ToString
def CliqueId := UInt16 deriving BEq, Hashable, OfNat, LE, Inhabited, ToString

def d2 (p: Pos) (q: Pos): Nat :=
  (p.x - q.x)^2 + (p.y - q.y)^2 + (p.z - q.z)^2 |>.toNat

def parsePos (s: String): Pos := 
  let xs := s.splitOn "," |>.map (·.toNat!) |>.toArray
  if h : xs.size = 3 then ⟨xs[0], xs[1], xs[2]⟩ else panic! "wrong dimension"

def opairs: List T -> List (T × T)
| [] => []
| x::xs => xs.map (x, ·) ++ opairs xs

def connectionCount := 1000
def N := 3

def countOccurrences (xs: List Nat): Std.HashMap Nat Nat :=
  xs.foldl (init := {}) fun m x =>
    m.alter x fun prevCount => some (prevCount.getD 0 + 1)

def main : IO Unit := do
  let contents <- IO.FS.readFile "input8.txt"
  let positions := contents.trim.splitOn "\n" |>.map parsePos
  let verts: List VertId := List.range positions.length |>.map (·.toUInt16)
  let mut mapping: HashMap VertId CliqueId := verts.map (fun i => (i,i)) |> HashMap.ofList
  let mut sortedPairs := positions.zip verts
    |> opairs
    |>.map (fun ((p1, i), (p2, j)) => (d2 p1 p2, (i, j)))
    |>.mergeSort (·.fst <= ·.fst)
  for (_, (i, j)) in sortedPairs do
    let ci := mapping[i]!
    let cj := mapping[j]!
    dbg_trace s!"{i} ∈ {ci}, {j} ∈ {cj}"
    if ci != cj then
      mapping := mapping.map (fun _ c => if c == cj then ci else c)
      let cc := mapping.values.eraseDups.length
      dbg_trace s!"=> {ci}. {cc} cliques."
      if cc == 1 then
        IO.println <| positions[i.toNat]!.x * positions[j.toNat]!.x
        break
