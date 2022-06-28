assume substring : 
  (s:string) -> 
  {i:int | i >= 0 /\ i < length(s)} ->
  {n:int | n >= 0 /\ n <= length(s)-i} ->
  {t:string | t = substring(s,i,n)}

assume match : (s:string) -> (t:string) -> {b:bool|b <=> s = t}
assume assert : {b:bool|b} -> unit

define f : string -> unit 
= \s:string.
  let zero = 0 in
  let one = 1 in
  let x = substring s zero one in
  let a = "a" in
  let p1 = match x a in
  if p1 then
    let y = substring s one one in
    let b = "b" in
    let p2 = match y b in
    assert p2
  else
    let z = substring s zero one in
    let c = "c" in
    let p3 = match z c in
    assert p3
