import axioms

f082 : {s:string|?} -> unit
f082 = \s:string.
  let n = length s in
  let p = le n 1 in
  let _ = assert p in
  let c = charAt s 0 in
  let p = eqChar c 'a' in
  assert p
