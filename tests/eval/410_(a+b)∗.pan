import axioms

f410 : {s:string|?} -> unit
f410 = \s:string.
  let n = length s in
  rec w : {i:int|?} -> unit = \i:int.
    let q = lt i n in
    if q then
      let c = charAt s i in
      let pa = eqChar c 'a' in
      let pb = eqChar c 'b' in
      let p = or pa pb in
      let _ = assert p in
      let i2 = add i 1 in
      w i2
    else
      unit
  in
    w 0
