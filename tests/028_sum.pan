leq : (a:int) -> (b:int) -> {c:bool| c = true <=> a <= b}
sub : (a:int) -> (b:int) -> {c:int|c = a - b}
add : (a:int) -> (b:int) -> {c:int|c = a + b}

sum : (n:int) -> {v:int| v >= 0 /\ n <= v}
sum = \n:int.
  let c = leq n 0 in
  if c then
    0
  else
    let n1 = sub n 1 in
    let t1 = sum n1 in
    add n t1
