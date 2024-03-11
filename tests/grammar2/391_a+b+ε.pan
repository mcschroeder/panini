import axioms

f391 : {s:string|?} -> unit
f391 = \s:string.
  let n = length s in
  let p0 = eq n 0 in
  if p0 then
    unit
  else
    let p1 = eq n 1 in
    let _ = assert p1 in
    let c0 = charAt s 0 in
    let pa = eqChar c0 'a' in
    if pa then
      unit
    else
      let pb = eqChar c0 'b' in
      if pb then
        unit
      else
        assert false
