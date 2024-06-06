import axioms

f520 : {s:string|?} -> unit
f520 = \s:string.
  let c0 = charAt s 0 in
  let c1 = charAt s 1 in
  let p0 = eqChar c0 'a' in  
  let p1 = eqChar c1 c0 in
  let _ = assert p0 in
  let _ = assert p1 in
  unit
