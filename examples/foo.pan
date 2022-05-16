assume length : (s:string) -> {n:int| n >= 0 /\ n = length(s) }
assume le : (a:int) -> (b:int) -> {c:bool| c <=> a < b}
assume add : (a:int) -> (b:int) -> {c:int|c=a+b}
assume assert : {b:bool|b} -> unit

assume substr : 
  { s : string | true } ->
  { i : int | i >= 0 /\ i < length(s) } ->
  { j : int | j >= 0 /\ j >= i /\ j < length(s) } ->
  { t : string | t = substring(s,i,j) }

assume eqstr : (s:string) -> (t:string) -> {b:bool | b <=> s = t}

assume foo : (s:string) -> unit
define foo = \s.
  rec L1 : int -> unit = \i1.
    let v1 = length s in
    let v2 = le i1 v1 in
    if v2 then
      let v3 = substr s i1 i1 in
      let v4 = eqstr v3 "a" in
      let _ = assert v4 in
      let i2 = add 1 i1 in
      L1 i2
    else
      unit
  in
  L1 0
