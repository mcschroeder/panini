import axioms

g1 : {s:string| s = "a"}

f161 : {s:string|?} -> unit
f161 = \s:string.
  let t = g1 in
  let p = match s t in
  let p2 = not p in
  let _ = assert p2 in
  let n = length s in
  let p3 = le n 1 in
  assert p3
