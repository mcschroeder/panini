assume assert : {b:bool|b} -> unit

assume match : 
  (s:string) -> 
  {i:int|i >= 0 /\ i < length(s)} -> 
  (t:string) -> 
  {b:bool|b <=> substring(s,i,1) = t}

assume bar : s:string -> unit
define bar = \s.
  let x = match s 0 "a" in
  assert x


assume main : unit
define main = 
  let bbb = "a" : {s:string|length(s) = 1 /\ substring(s,0,1) = "a"} in
  let x = match bbb 0 "a" in
  assert x

