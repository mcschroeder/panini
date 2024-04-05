import axioms

f120 : {s:string|?} -> unit
f120 = \s:string.
  let x = charAt s 0 in
  let p = eqChar x 'a' in
  assert p
