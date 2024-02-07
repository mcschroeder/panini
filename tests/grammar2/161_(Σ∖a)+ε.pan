import axioms

g1 : {s:string| s = "a"}

f161 : {s:string|?} -> unit
f161 = \s:string.
  let t = g1 in
  let p = match s t in
  let p2 = not p in
  assert p2
