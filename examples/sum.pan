assume leq : n:int -> m:int -> {b:bool|b <=> n <= m}
assume sub : a:int -> b:int -> {c:int|c = a - b}
assume add : a:int -> b:int -> {c:int|c = a + b}

assume sum : n:int -> {m:int|m >= 0 /\ n <= m}
define sum = \n.
  let c = leq n 0 in
  if c then 0
  else
    let n1 = sub n 1 in
    let t1 = sum n1 in
    add n t1
