import axioms

f413 : {s:string|?} -> unit
f413 = \s:string.
  let n = length s in
  rec w : int -> unit = \i:int.
    let q = lt i n in
    if q then
      let c = charAt s i in
      let pa = eqChar c 'a' in      
      let qa = not pa in
      if qa then
        assert false
      else
        let pb = eqChar c 'b' in
        let qb = not pb in
        if qb then
          assert false
        else
          let i2 = add i 1 in
          w i2
    else
      unit
  in
    w 0
