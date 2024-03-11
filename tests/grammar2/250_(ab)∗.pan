import axioms

f250 : {s:string|?} -> unit
f250 = \s:string.
  let n = length s in
  rec w : int -> unit = \i0:int.
    let p0 = ge i0 n in
    if p0 then
      unit
    else
      let c1 = charAt s i0 in
      let p1 = eqChar c1 'a' in
      let _  = assert p1 in
      let i1 = add i0 1 in
      let c2 = charAt s i1 in
      let p2 = eqChar c2 'b' in
      let _  = assert p2 in
      let i2 = add i1 1 in
      w i2
  in
    w 0
