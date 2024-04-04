import axioms

f291 : {s:string|?} -> unit
f291 = \s:string.
  let n = length s in
  let p0 = eq n 0 in
  if p0 then
    unit
  else
    let c1 = charAt s 0 in
    let p1 = eqChar c1 'a' in
    if p1 then
      let p2 = eq n 1 in
      if p2 then
        unit
      else
        let c2 = charAt s 1 in
        let p3 = eqChar c2 'b' in
        let _ = assert p3 in
        let p4 = eq n 2 in
        assert p4
    else
      let p5 = eqChar c1 'b' in
      let _ = assert p5 in
      let p6 = eq n 1 in
      assert p6
