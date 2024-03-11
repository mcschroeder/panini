import axioms

f160 : {s:string|?} -> unit
f160 = \s:string.
  let n = length s in
  let p1 = eq n 0 in
  if p1 then
    unit
  else
    let p2 = eq n 1 in
    let _ = assert p2 in
    let x = charAt s 0 in
    let p3 = eqChar x 'a' in
    let p4 = not p3 in
    assert p4
