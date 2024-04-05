import axioms

f401 : {s:string|?} -> unit
f401 = \s:string.
  let n = length s in
  rec w : int -> char -> bool = \i:int.\c:char.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = eqChar x c in
      if p2 then
        let i2 = add i 1 in
        w i2 c
      else
        false
    else
      true
  in
    let pa = w 0 'a' in
    let pb = w 0 'b' in
    let p = or pa pb in
    assert p
