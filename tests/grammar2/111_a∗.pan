import axioms

f111 : {s:string|?} -> unit
f111 = \s:string.
  let n = length s in
  rec w : int -> bool = \i:int.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let q1 = match x "a" in
      let i2 = add i 1 in
      let q2 = w i2 in
      and q1 q2
    else
      unit
  in
    let q = w 0 in
    assert q
