assume assert : { b:bool | b = true } -> unit
assume equals : (a:int) -> (b:int) -> { c:bool | c = true <=> a = b }
assume length : (s:string) -> { n:int | n >= 0 /\ n = |s| }
assume charAt : (s:string) -> { i:int | i >= 0 /\ i < |s| } -> { t:string | t = s[i] }
assume match  : (s:string) -> (t:string) -> { b:bool | b = true <=> s = t }

define f : string -> unit =
  \s:string.
    let n = length s in
    let p1 = equals n 1 in
    if p1 then
      let x = charAt s 0 in
      let p2 = match x "a" in
      assert p2
    else
      let y = charAt s 1 in
      let p3 = match y "b" in
      assert p3
