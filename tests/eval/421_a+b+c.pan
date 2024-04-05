import axioms

f421 : {s:string|?} -> unit
f421 = \s:string.
  let c = charAt s 0 in
  let n = length s in
  let q = eq n 1 in
  let _ = assert q in
  let p1 = eqChar c 'a' in
  if p1 then
    unit
  else
    let p2 = eqChar c 'b' in
    if p2 then
      unit
    else
      let p3 = eqChar c 'c' in
      if p3 then
        unit
      else
        assert false
