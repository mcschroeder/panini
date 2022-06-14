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

define f = \s:string.
  let n = length s in
  rec L1 : int -> int -> unit = \i1:int. \c1:int.
    let v1 = lt i1 n in
    if v1 then
      let v2 = substring s i1 1 in
      let i2 = add i1 1 in
      let v3 = match v2 "(" in
      if v3 then
        let c2 = add c1 1 in
        L1 i2 c2
      else
        let v4 = match v2 ")" in
        if v4 then
          let c2 = sub c1 1 in
          L1 i2 c2
        else
          L1 i2 c1
    else
      let v5 = eq c1 0 in
      assert v5
  in L1 0 0