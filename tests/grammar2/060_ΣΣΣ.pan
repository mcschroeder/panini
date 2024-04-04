import axioms

f060 : {s:string|?} -> unit
f060 = \s:string.
  let n = length s in
  let p = eq n 3 in
  assert p
