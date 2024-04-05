import axioms

f310 : {s:string|?} -> unit
f310 = \s:string.
  let n = length s in
  rec w : int -> unit = \(i:int).
    let p = lt i n in
    if p then
      let x = charAt s i in
      let m1 = eqChar x 'a' in
      let i2 = add i 1 in
      if m1 then
        let y = charAt s i2 in
        let m2 = eqChar y 'b' in
        let _ = assert m2 in
        let i3 = add i2 1 in
        let m3 = eq i3 n in
        assert m3
      else
        w i2
    else
      assert false
  in
    w 0
