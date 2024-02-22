import axioms

f012 : {s:string|?} -> unit
f012 = \s:string.
  let n = length s in
  let p = le n 1 in
  let _ = assert p in
  let _ = charAt s 0 in
  unit