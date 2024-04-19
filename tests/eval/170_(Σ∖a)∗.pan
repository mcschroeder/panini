import axioms

f170 : {s:string|?} -> unit
f170 = \s:string.
  let n = length s in
  rec w : {i:int|?} -> unit = \i:int.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = eqChar x 'a' in
      let p3 = not p2 in
      let _ = assert p3 in
      let i2 = add i 1 in
      w i2
    else
      unit
  in
    w 0