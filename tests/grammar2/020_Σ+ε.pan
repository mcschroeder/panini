import axioms

f020 : {s:string|?} -> unit
f020 = \s:string.
  let n = length s in
  let p = le n 1 in
  assert p
