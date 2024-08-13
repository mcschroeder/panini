import axioms

f220 : {s:string|?} -> unit
f220 = \s:string.
  let n = length s in
  rec w : {i:int|?} -> int = \i:int.
    let p1 = ge i n in
    if p1 then
      let _ = assert false in 0
    else
      let c = charAt s i in
      let p = eqChar c 'a' in
      if p then
        let i2 = add i 1 in
        w i2
      else
        i
  in
    let i = w 0 in
    let m = sub n 1 in
    let p1 = eq i m in
    let c = charAt s i in
    let p2 = eqChar c 'b' in
    assert p2
