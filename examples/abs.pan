assume geq : n:int -> m:int -> {b:bool|b <=> n >= m}
assume sub : a:int -> b:int -> {c:int|c = a - b}

assume abs : x:int -> {y:int| y >= 0}
define abs = \x.
  let pos = geq x 0 in
  if pos then 
    x 
  else 
    sub 0 x
