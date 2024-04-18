import axioms

f040 : {s:string|?} -> unit
f040 = \s:string.
  let n = length s in
  let p = eq n 2 in
  assert p
