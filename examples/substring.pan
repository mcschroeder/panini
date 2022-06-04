assume eq : a:int -> b:int -> {c:bool| c <=> a = b}
assume lt : a:int -> b:int -> {c:bool| c <=> a < b}
assume add : a:int -> b:int -> {c:int| c = a + b}
assume sub : a:int -> b:int -> {c:int| c = a - b}
assume assert : {b:bool|b} -> unit

assume length : s:string -> {n:int| n >= 0 /\ n = length(s)}

assume substring :
  { s : string | length(s) > 0 } ->
  { i : int | i >= 0 /\ i < length(s) } ->
  { t : string | t = substring(s,i) }

assume match : s:string -> t:string -> {b:bool|b <=> s = t}

assume bar : s:string -> unit
define bar = \s.
  rec L1 : int -> unit = \i1.
    let v0 = length s in
    let v1 = lt i1 v0 in
    if v1 then      
      let v2 = substring s i1 in
      let v3 = match v2 "a" in
      if v3 then
        let i2 = add i1 1 in
        L1 i2
      else
        unit
    else
      unit
  in
    L1 0