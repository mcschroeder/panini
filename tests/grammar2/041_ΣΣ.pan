import axioms

f041 : {s:string|?} -> unit
f041 = \s:string.
  let n = length s in
  let p = le n 2 in
  let _ = assert p in
  let _ = charAt s 0 in 
  let _ = charAt s 1 in
  unit
