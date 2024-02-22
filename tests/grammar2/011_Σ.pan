import axioms

f011 : {s:string|?} -> unit
f011 = \s:string.
  let _ = charAt s 0 in
  let n = length s in
  let p = le n 1 in
  assert p
