import axioms

f141 : {s:string|?} -> unit
f141 = \s:string.
  let n = length s in
  rec w : {i:int|?} -> unit = \i:int.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = eqChar x 'a' in
      if p2 then
        unit
      else
        let i2 = add i 1 in
        w i2
    else
      assert false
  in
    w 0
