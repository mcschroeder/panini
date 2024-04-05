import axioms

lsb_check : {s1:string|?} -> unit
lsb_check = \s1:string.
  let v1 = length s1 in
  let v2 = sub v1 1 in
  rec w1 : int -> unit = \i1:int.
    let v3 = lt i1 v2 in
    if v3 then
      let v4 = charAt s1 i1 in
      let v5 = eqChar v4 '0' in
      let _ = assert v5 in
      let i2 = add i1 1 in
      w1 i2
    else
      unit
  in
    let _ = w1 0 in
    let v6 = charAt s1 v2 in 
    let v7 = eqChar v6 '1' in
    assert v7
