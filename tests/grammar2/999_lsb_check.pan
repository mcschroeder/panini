import axioms

lsb_check : {s:string|?} -> unit
lsb_check = \s:string.
  let n = length s in
  let n1 = sub n 1 in
  rec w : int -> unit = \i:int.
    let p1 = lt i n1 in
    if p1 then
      let bit = charAt s i in
      let m0 = eqChar bit '0' in
      let _ = assert m0 in
      let i2 = add i 1 in
      w i2
    else
      unit
  in
    let _ = w 0 in
    let lsb = charAt s n1 in 
    let m1 = eqChar lsb '1' in
    assert m1
