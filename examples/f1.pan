assume substring : 
  (s:string) -> 
  {i:int | i >= 0 /\ i < length(s)} ->
  {t:string | t = substring(s,i)}

assume match : (s:string) -> (t:string) -> {b:bool|b <=> s = t}
assume assert : {b:bool|b} -> unit

assume f : string -> unit
define f = \s:{v:string|v = "abc"}.
  let x = substring s 0 in
  let p = match x "a" in
  let _ = assert p in
  unit