import axioms

f490 : {s:string|?} -> unit
f490 = \s:string.
  let n = length s in
  rec w : int -> unit = \i:int.
    let q = lt i n in
    if q then
      let c = charAt s i in
      let pa = eqChar c 'a' in
      let pb = eqChar c 'b' in
      let p = or pa pb in
      let _ = assert p in
      let i2 = add i 1 in
      w i2
    else
      unit
  in
    let _ = w 0 in
    let i = sub n 3 in
    let j = sub n 1 in
    let t = slice s i j in
    let p = match t "abb" in
    assert p
    
