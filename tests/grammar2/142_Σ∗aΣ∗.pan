import axioms

f142 : {s:string|?} -> unit
f142 = \s:string.
  let n = length s in
  rec w : int -> bool = \i:int.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = match x "a" in
      if p2 then
        true
      else
        let i2 = add i 1 in
        w i2
    else
      false
  in
    let p3 = w 0 in
    assert p3
