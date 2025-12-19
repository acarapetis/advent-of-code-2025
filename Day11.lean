import Std.Data.HashMap

abbrev Name := String

partial def routeCount (links: Std.HashMap Name (List Name)) (source: Name): StateM (Std.HashMap Name Nat) Nat := do
  let cache <- get
  match cache.get? source with
  | some result => return result
  | none =>
    let result <-
      if source == "out" then
        modify (·.insert source 1)
        return 1
      else
        let mut sum: Nat := 0
        for link in links.get! source do
          let c: Nat <- routeCount links link
          sum := sum + c
        modify (·.insert source sum)
        return sum

def parseLine (s: String): Name × List Name :=
  if let x::xs := s.splitOn ": " then
    (x, xs[0]!.splitOn " ")
  else
    panic! "bad line"

def main: IO Unit := do
  let content <- IO.FS.readFile "input11.txt"
  let lines := content.trim.splitOn "\n"
  let links := lines.map parseLine |> Std.HashMap.ofList
  routeCount links "you" |>.run' Std.HashMap.emptyWithCapacity |> IO.println
