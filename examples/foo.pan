assume length : (s:string) -> {n:int| n >= 0 /\ n = length(s) }
assume le : (a:int) -> (b:int) -> {c:bool| c <=> a < b}
assume add : (a:int) -> (b:int) -> {c:int|c=a+b}
assume assert : {b:bool|b} -> unit

assume match : 
  (s:string) -> 
  {i:int|i >= 0 /\ i < length(s)} -> 
  (t:string) -> 
  {b:bool|b <=> index(s,i) = t}

assume foo : string -> unit
define foo = \s.
  let i0 = 0 in
  rec L1 : int -> unit = \i1.
    let v1 = length s in
    let v2 = le i1 v1 in
    if v2 then
      let v3 = match s i1 "a" in      
      let _ = assert v3 in
      let i2 = add 1 i1 in
      L1 i2
    else
      unit
  in
  L1 i0
