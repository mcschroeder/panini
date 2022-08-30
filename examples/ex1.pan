assume assert : { b:bool | b = true } -> unit
assume equals : (a:int) -> (b:int) -> { c:bool | c = true <=> a = b }
assume length : (s:string) -> { n:int | n >= 0 /\ n = |s| }
assume charAt : (s:string) -> { i:int | i >= 0 /\ i < |s| } -> { t:string | t = s[i] }
assume match  : (s:string) -> (t:string) -> { b:bool | b = true <=> s = t }

define f : {s:string|?} -> unit =
  \s:string.
    let c1 = 0 in
    let x = charAt s c1 in
    let c2 = "a" in
    let p1 = match x c2 in
    if p1 then
      let n = length s in
      let c3 = 1 in
      let p2 = equals n c3 in
      assert p2
    else
      let c4 = 1 in
      let y = charAt s c4 in
      let c5 = "b" in
      let p3 = match y c5 in
      assert p3
