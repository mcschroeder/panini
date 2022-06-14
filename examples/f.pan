assume substring : 
  (s:string) -> 
  {i:int | i >= 0 /\ i < length(s)} ->
  {n:int | n >= 0 /\ n <= length(s)-i} ->
  {t:string | t = substring(s,i,n)}

assume match : (s:string) -> (t:string) -> {b:bool|b <=> s = t}
assume assert : {b:bool|b} -> unit

assume f : string -> unit
define f = \s:string.
  let x = substring s 0 1 in
  let p = match x "a" in
  assert p