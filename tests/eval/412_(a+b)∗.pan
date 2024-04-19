import axioms

f412 : {s:string|?} -> unit
f412 = \s:string.
  let n = length s in
  rec w : {i:int|?} -> unit = \i:int.
    let q = lt i n in
    if q then
      let c = charAt s i in
      let pa = eqChar c 'a' in
      if pa then
        let i2 = add i 1 in
        w i2
      else
        let pb = eqChar c 'b' in
        if pb then
          let i2 = add i 1 in
          w i2
        else
          assert false
    else
      unit
  in
    w 0
