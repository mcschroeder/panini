import axioms

f081 : {s:string|?} -> unit
f081 = \s:string.
  let n = length s in
  let p = eq n 1 in
  let _ = assert p in
  let c = charAt s 0 in
  let p = eqChar c 'a' in
  assert p
