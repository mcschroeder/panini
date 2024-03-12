import axioms

f402 : {s:string|?} -> unit
f402 = \s:string.
  let n = length s in
  rec wA : int -> bool = \i:int.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = eqChar x 'a' in
      if p2 then
        let i2 = add i 1 in
        wA i2
      else
        false
    else
      true
  in
  rec wB : int -> bool = \i:int.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = eqChar x 'b' in
      if p2 then
        let i2 = add i 1 in
        wB i2
      else
        false
    else
      true
  in
    let pa = wA 0 in
    let pb = wB 0 in
    let p = or pa pb in
    assert p
