assume geq : n:int -> m:int -> {b:bool|b <=> n >= m}
assume sub : a:int -> b:int -> {c:int|c = a - b}

assume abs : x:int -> {y:int| ? }
define abs = \x.
  let c = geq x 0 in
  if c then 
    x 
  else 
    sub 0 x
