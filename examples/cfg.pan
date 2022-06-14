assume substring : 
  (s:string) -> 
  {i:int | i >= 0 /\ i < length(s)} ->
  {n:int | n >= 0 /\ n <= length(s)-i} ->
  {t:string | t = substring(s,i,n)}

assume length : (s:string) -> {n:int|n=length(s)}

assume match : (s:string) -> (t:string) -> {b:bool|b <=> s = t}
assume assert : {b:bool|b} -> unit

assume add : (a:int) -> (b:int) -> {c:int|c = a + b}
assume sub : (a:int) -> (b:int) -> {c:int|c = a - b}
assume eq : (a:int) -> (b:int) -> {c:bool|c <=> a = b}
assume lt : (a:int) -> (b:int) -> {c:bool|c <=> a < b}

define f = \s.
  let v1 = length s in
  rec L2 : int -> int -> unit = \i1:int. \n1:int.
    let v2 = lt i1 v1 in
    if v2 then
      let v3 = substring s i1 1 in
      let v4 = match v3 "b" in
      let _ = assert v4 in
      let i2 = add i1 1 in
      let n2 = sub n1 1 in
      L2 i2 n2
    else
      let v5 = eq n1 0 in
      assert v5
  in
    rec L1 : int -> unit = \i1:int.
      let v6 = substring s i1 1 in
      let v7 = match v6 "a" in
      if v7 then
        let i2 = add i1 1 in
        L1 i2
      else
        L2 i1 i1
    in
      L1 0