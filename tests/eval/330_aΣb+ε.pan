import axioms

f330 : {s:string|?} -> unit
f330 = \s:string.
  let n = length s in
  let p0 = eq n 0 in
  if p0 then
    unit
  else
    let p3 = eq n 3 in
    if p3 then
      let c0 = charAt s 0 in
      let c2 = charAt s 2 in
      let pa = eqChar c0 'a' in
      let pb = eqChar c2 'b' in
      let _  = assert pa in
      let _  = assert pb in
      unit
    else
      assert false
