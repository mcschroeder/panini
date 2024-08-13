import axioms

f380 : {s:string|?} -> unit
f380 = \s:string.
  let p1 = match s "" in
  let p2 = match s "b" in
  let p3 = or p1 p2 in
  if p3 then
    unit
  else
    let n = length s in
    rec w : {i:int|?} -> unit = \i:int.
      let q = lt i n in
      if q then
        let c = charAt s i in
        let p = eqChar c 'a' in
        let _ = assert p in
        let i2 = add i 1 in
        w i2
      else
        unit
    in
      w 0
