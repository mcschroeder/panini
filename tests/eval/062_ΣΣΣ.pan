import axioms

f062 : {s:string|?} -> unit
f062 = \s:string.
  let n = length s in
  let p = le n 3 in
  let _ = assert p in
  let i1 = sub n 1 in
  let _ = charAt s i1 in
  let i2 = sub i1 1 in
  let _ = charAt s i2 in
  let i3 = sub i2 1 in
  let _ = charAt s i3 in
  unit
