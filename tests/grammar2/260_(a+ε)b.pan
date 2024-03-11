import axioms

f260 : {s:string|?} -> unit
f260 = \s:string.
  let p1 = match s "ab" in
  let p2 = match s "b" in
  let p3 = or p1 p2 in
  assert p3

f261 : {s:string|?} -> unit
f261 = \s:string.
  let n = length s in
  let p0 = le n 2 in
  if p0 then    
    let c1 = charAt s 0 in
    let p1 = eqChar c1 'a' in
    if p1 then
      let c2 = charAt s 1 in
      let p2 = eqChar c2 'b' in
      assert p2
    else
      let p3 = eqChar c1 'b' in
      let _ = assert p3 in
      let p4 = eq n 1 in
      assert p4
  else
    assert false
