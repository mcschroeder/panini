import axioms

f200 : {s:string|?} -> unit
f200 = \s:string.
  let n = length s in
  let m = sub n 1 in
  rec w : {i:int|?} -> unit = \i:int.
    let p = lt i m in
    if p then
      let x = charAt s i in
      let p2 = eqChar x 'a' in
      let _ = assert p2 in
      let i2 = add i 1 in
      w i2
    else
      let y = charAt s i in
      let p3 = eqChar y 'a' in
      let p4 = not p3 in
      assert p4
  in
    w 0
