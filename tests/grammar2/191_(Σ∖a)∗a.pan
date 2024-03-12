import axioms

f191 : {s:string|?} -> unit
f191 = \s:string.  
  rec w : int -> unit = \i:int.
    let p = ge i 0 in
    if p then
      let x = charAt s i in
      let p2 = eqChar x 'a' in
      let p3 = not p2 in
      let _ = assert p3 in
      let i2 = sub i 1 in
      w i2
    else
      unit
  in
    let n = length s in
    let m = sub n 1 in
    let y = charAt s m in
    let p1 = eqChar y 'a' in
    let _ = assert p1 in
    let i0 = sub m 1 in
    w i0
