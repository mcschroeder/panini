import axioms

f063 : {s:string|?} -> unit
f063 = \s:string.
  let n = length s in
  let p = le n 3 in
  let _ = assert p in
  let i1 = 0 in
  let _ = charAt s i1 in
  let i2 = add i1 1 in
  let _ = charAt s i2 in
  let i3 = add i2 1 in
  let _ = charAt s i3 in
  unit