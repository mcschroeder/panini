import axioms

f120 : {s:string|?} -> unit
f120 = \s:string.
  let c = charAt s 0 in
  let p = eqChar c 'a' in
  assert p
