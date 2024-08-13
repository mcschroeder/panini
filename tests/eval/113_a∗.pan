import axioms

f113 : {s:string|?} -> unit
f113 = \s:string.  
  rec w : {i:int|?} -> unit = \i:int.
    let n = length s in
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = eqChar x 'a' in
      let _ = assert p2 in
      let i2 = add i 1 in
      w i2
    else
      unit
  in
    w 0
