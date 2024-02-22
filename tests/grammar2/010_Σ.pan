import axioms

f010 : {s:string|?} -> unit
f010 = \s:string.
  let n = length s in
  let p = eq n 1 in
  assert p
