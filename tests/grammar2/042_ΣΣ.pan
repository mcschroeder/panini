import axioms

f042 : {s:string|?} -> unit
f042 = \s:string.
  let n = length s in
  let p = le n 2 in
  let _ = assert p in
  let i1 = sub n 1 in
  let _ = charAt s i1 in
  let i2 = sub i1 1 in
  let _ = charAt s i2 in
  unit
