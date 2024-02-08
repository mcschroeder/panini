import axioms

f230 : {s:string|?} -> unit
f230 = \s:string.
  let c = charAt s 0 in
  let p1 = match c "a" in
  let _ = assert p1 in
  let n = length s in
  rec w : int -> unit = \i:int.
    let p2 = lt i n in
    if p2 then
      let x = charAt s i in
      let p2 = match x "b" in
      let _ = assert p2 in
      let i2 = add i 1 in
      w i2
    else
      unit
  in
    w 1
