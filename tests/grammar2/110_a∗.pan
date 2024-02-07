import axioms

f110 : {s:string|?} -> unit
f110 = \s:string.
  let n = length s in
  rec w : int -> unit = \i:int.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = match x "a" in
      let _ = assert p2 in
      let i2 = add i 1 in
      w i2
    else
      unit
  in
    w 0
