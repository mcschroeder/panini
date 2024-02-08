import axioms

f340 : {s:string|?} -> unit
f340 = \s:string.
  let n = length s in
  let go = \i0:int.
    let c0 = charAt s i0 in
    let p0 = match c0 "a" in
    let i2 = add i0 2 in
    let c2 = charAt s i2 in
    let p2 = match c2 "b" in
    let p3 = and p0 p2 in
    assert p3
  in
    rec w : int -> unit = \i:int.
      let q = lt i n in
      if q then
        let _ = go i in
        let i' = add i 3 in
        w i'
      else
        unit
    in
      w 0
