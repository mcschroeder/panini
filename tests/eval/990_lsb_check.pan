import axioms

lsb_check : {s:string|?} -> unit
lsb_check = \s:string.
  let n1 = length s in
  let n2 = sub n1 1 in
  rec w1 : int -> unit = \i1:int.
    let p1 = lt i1 n2 in
    if p1 then
      let c1 = charAt s i1 in
      let p2 = eqChar c1 '0' in
      let _ = assert p2 in
      let i2 = add i1 1 in
      w1 i2
    else
      unit
  in
    let _ = w1 0 in
    let c2 = charAt s n2 in
    let p3 = eqChar c2 '1' in
    assert p3
