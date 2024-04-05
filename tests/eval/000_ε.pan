import axioms

f000 : {s:string|?} -> unit
f000 = \s:string.
  let n = length s in
  let p = eq n 0 in
  assert p
