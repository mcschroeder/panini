import axioms

f222 : {s:string|?} -> unit
f222 = \s:string.
  let n = length s in
  rec w : {i:int|?} -> unit = \i:int.
    let m = sub n 1 in
    let p = lt i m in
    if p then
      let x = charAt s i in
      let p2 = eqChar x 'a' in
      let _ = assert p2 in
      let i2 = add i 1 in
      w i2
    else
      unit
  in
    let j = sub n 1 in
    let c = charAt s j in
    let p3 = eqChar c 'b' in
    let _ = assert p3 in
    w 0
