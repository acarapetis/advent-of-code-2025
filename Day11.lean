import Std.Data.HashMap

abbrev Name := String

partial def routeCount' (links: Std.HashMap Name (List Name)) (source: Name) (target: Name): StateM (Std.HashMap Name Nat) Nat := do
  let cache <- get
  match cache.get? source with
  | some result => return result
  | none =>
    let result <-
      if source == target then
        modify (·.insert source 1)
        return 1
      else
        let mut sum: Nat := 0
        for link in links.getD source [] do
          let c: Nat <- routeCount' links link target
          sum := sum + c
        modify (·.insert source sum)
        return sum

def routeCount (links: Std.HashMap Name (List Name)) (source: Name) (target: Name): Nat :=
  routeCount' links source target |>.run' Std.HashMap.emptyWithCapacity

def parseLine (s: String): Name × List Name :=
  if let x::xs := s.splitOn ": " then
    (x, xs[0]!.splitOn " ")
  else
    panic! "bad line"

def main: IO Unit := do
  let content <- IO.FS.readFile "input11.txt"
  let lines := content.trim.splitOn "\n"
  let links := lines.map parseLine |> Std.HashMap.ofList
  let rc := routeCount links
  IO.print "Part 1: "
  rc "you" "out" |> IO.println
  IO.print "Part 2: "
  let path1 := (rc "svr" "fft") * (rc "fft" "dac") * (rc "dac" "out")
  let path2 := (rc "svr" "dac") * (rc "dac" "fft") * (rc "fft" "out")
  IO.println <| path1 + path2
