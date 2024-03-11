assert : { b:bool | b = true } -> unit
equals : (a:int) -> (b:int) -> { c:bool | c = true <=> a = b }
length : (s:string) -> { n:int | n >= 0 /\ n = |s| }
charAt : (s:string) -> { i:int | i >= 0 /\ i < |s| } -> { t:char | t = s[i] }
match  : (s:char) -> (t:char) -> { b:bool | b = true <=> s = t }

f = \s:string.
    let n = length s in
    let p1 = equals n 1 in
    if p1 then
      let x = charAt s 0 in
      let p2 = match x 'a' in
      assert p2
    else
      let y = charAt s 1 in
      let p3 = match y 'b' in
      assert p3
