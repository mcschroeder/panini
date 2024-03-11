import axioms

f320 : {s:string|?} -> unit
f320 = \s:string.
  let c0 = charAt s 0 in
  let p0 = eqChar c0 'a' in
  let _  = assert p0 in
  let c2 = charAt s 2 in
  let p2 = eqChar c2 'b' in
  let _  = assert p2 in
  let n  = length s in
  let pn = eq n 3 in
  let _  = assert pn in
  unit
