import axioms

f192 : {s:string|?} -> unit
f192 = \s:string.
  let n = length s in
  let m = sub n 1 in
  rec w : int -> unit = \i:int.
    let p = lt i m in
    if p then
      let x = charAt s i in
      let p2 = match x "a" in
      let p3 = not p2 in
      let _ = assert p3 in
      let i2 = add i 1 in
      w i2
    else
      let y = charAt s i in
      let p4 = match y "a" in
      assert p4
  in
    w 0