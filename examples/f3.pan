assume substring : 
  (s:string) -> 
  {i:int | i >= 0 /\ i < length(s)} ->
  {n:int | n >= 0 /\ n <= length(s)-i} ->
  {t:string | t = substring(s,i,n)}

assume length : (s:string) -> {n:int|n>= 0 /\ n=length(s)}

assume match : (s:string) -> (t:string) -> {b:bool|b <=> s = t}
assume assert : {b:bool|b} -> unit
assume eq : (a:int) -> (b:int) -> {c:bool|c <=> a = b}

define f = \s.  
  let x = substring s 0 1 in
  let p1 = match x "a" in
  if p1 then
    let n = length s in
    let p2 = eq n 1 in
    assert p2
  else
    let y = substring s 1 1 in
    let p3 = match y "b" in
    assert p3
