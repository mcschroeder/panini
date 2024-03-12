import axioms

f500 : {s:string|?} -> unit
f500 = \s:string.
  let n = length s in
  rec w : int -> char -> int = \i:int. \c:char.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = eqChar x c in
      if p2 then
        let i2 = add i 1 in
        w i2 c
      else
        i
    else
      i
  in
    let i1 = w 0 'a' in
    let i2 = w i1 'b' in
    let i3 = w i2 'c' in
    let p = eq i3 n in
    assert p
