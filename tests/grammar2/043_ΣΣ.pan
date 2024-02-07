import axioms

f043 : {s:string|?} -> unit
f043 = \s:string.
  let n = length s in
  let p = le n 2 in
  let _ = assert p in
  let i1 = 0 in
  let _ = charAt s i1 in
  let i2 = add i1 1 in
  let _ = charAt s i2 in
  unit