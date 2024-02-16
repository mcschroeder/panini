import axioms

f411 : {s:string|?} -> unit
f411 = \s:string.
  let n = length s in
  rec w : int -> bool = \i:int.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let qa = match x "a" in
      let qb = match x "b" in
      let q1 = or qa qb in
      let i2 = add i 1 in
      let q2 = w i2 in
      and q1 q2
    else
      false
  in
    let q = w 0 in
    assert q
