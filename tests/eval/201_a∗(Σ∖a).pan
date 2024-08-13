import axioms

f201 : {s:string|?} -> unit
f201 = \s:string.
  let n = length s in
  rec w : {i:int|?} -> int = \i:int.
    let p1 = lt i n in 
    if p1 then
      let c = charAt s i in
      let p2 = eqChar c 'a' in
      if p2 then
        let i2 = add i 1 in
        w i2
      else
        i
    else
      i
  in
    let i = w 0 in
    let m = sub n 1 in
    let p = eq i m in
    assert p
