assume assert : { b:bool | b = true } -> unit
assume equals : (a:int) -> (b:int) -> { c:bool | c = true <=> a = b }
assume length : (s:string) -> { n:int | n >= 0 /\ n = |s| }
assume charAt : (s:string) -> { i:int | i >= 0 /\ i < |s| } -> { t:string | t = s[i] }
assume match  : (s:string) -> (t:string) -> { b:bool | b = true <=> s = t }
assume sub : (a:int) -> (b:int) -> { c:int | c = a - b}

define f : string -> unit =
  \s:string.
    let x = charAt s 0 in
    let p1 = match x "a" in
    if p1 then
      let n = length s in
      let z = sub n 1 in
      let p2 = equals z 0 in
      assert p2
    else
      let y = charAt s 1 in
      let p3 = match y "b" in
      assert p3
