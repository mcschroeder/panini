import axioms

f031 : {s:string|?} -> unit
f031 = \s:string.
  let n = length s in
  let p = ge n 0 in
  assert p
