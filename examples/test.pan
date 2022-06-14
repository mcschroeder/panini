assume sub : (a:int) -> (b:int) -> {c:int|c=a-b /\ a=c+b /\ b=a-c}
assume geq : (a:int) -> (b:int) -> {c:bool|c <=> a >= b}
assume assert : {b:bool|b} -> unit

define f = \x:int.
  let y = sub x 5 in
  let p = geq y 0 in
  assert p