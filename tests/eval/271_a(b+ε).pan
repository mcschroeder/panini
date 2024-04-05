import axioms

f271 : {s:string|?} -> unit
f271 = \s:string.
  let c1 = charAt s 0 in
  let p1 = eqChar c1 'a' in
  let _ = assert p1 in
  let n = length s in
  let p2 = eq n 1 in
  if p2 then
    unit
  else
    let c2 = charAt s 1 in
    let p3 = eqChar c2 'b' in
    let _ = assert p3 in
    let p4 = eq n 2 in
    assert p4