import axioms

f430 : {s:string|?} -> unit
f430 = \s:string.
  let n = length s in
  let q = eq n 1 in
  let _ = assert q in
  let c = charAt s 0 in
  let pa = eqChar c 'a' in
  let pb = eqChar c 'b' in
  let pb0 = not pb in
  let pc = eqChar c 'c' in
  let p1 = or pa pb0 in
  let p2 = or p1 pc in
  assert p2
