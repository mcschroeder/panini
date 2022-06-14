assume substring : 
  (s:string) -> 
  {i:int | i >= 0 /\ i < length(s)} ->
  {t:string | t = substring(s,i)}

assume match : (s:string) -> (t:string) -> {b:bool|b <=> s = t}
assume assert : {b:bool|b} -> unit

assume length : (s:string) -> {n:int| n >= 0 /\ n = length(s) }
assume le : (a:int) -> (b:int) -> {c:bool| c <=> a < b}
assume add : (a:int) -> (b:int) -> {c:int|c=a+b}

assume f : string -> unit
define f = \s:string.
  rec L1 : {v:int|v>=0} -> unit = \i1:{v:int|v>=0}.
    let v1 = le i1 3 in
    if v1 then
      let v2 = substring s i1 in
      let v3 = match v2 "a" in
      let _ = assert v3 in
      let i2 = add i1 1 in
      L1 i2
    else
      unit
  in
    L1 0
