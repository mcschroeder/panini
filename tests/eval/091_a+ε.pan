import axioms

f091 : {s:string|?} -> unit
f091 = \s:string.
  let n = length s in
  let p1 = gt n 1 in
  let p2 = not p1 in
  let _ = assert p2 in
  let p3 = eq n 0 in
  if p3 then
    unit
  else
    let c = charAt s 0 in
    let p4 = eqChar c 'a' in
    assert p4
