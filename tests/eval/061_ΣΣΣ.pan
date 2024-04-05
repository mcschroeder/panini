import axioms

f061 : {s:string|?} -> unit
f061 = \s:string.
  let n = length s in
  let p = le n 3 in
  let _ = assert p in
  let _ = charAt s 0 in
  let _ = charAt s 1 in
  let _ = charAt s 2 in
  unit
