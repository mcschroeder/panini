import axioms

f420 : {s:string|?} -> unit
f420 = \s:string.
  let p1 = match s "a" in 
  let p2 = match s "b" in 
  let p3 = match s "c" in
  let p4 = or p1 p2 in
  let p5 = or p3 p4 in
  assert p5
