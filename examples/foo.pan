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
      let v4 = "a" in
      let v5 = eqstr v3 v4 in
      let _ = assert v5 in
      let v6 = 1 in
      let i2 = add v6 i1 in
      L1 i2
    else
      unit
  in
  let v7 = 0 in
  L1 v7
