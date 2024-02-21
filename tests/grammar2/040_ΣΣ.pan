import axioms

f040 : {s:string|?} -> unit
f040 = \s:string.
  let n = length s in
  let p = eq n 2 in
  assert p

f041 : {s:string|?} -> unit
f041 = \s:string.
  let n = length s in
  let p = le n 2 in
  let _ = assert p in
  let _ = charAt s 0 in 
  let _ = charAt s 1 in
  unit
