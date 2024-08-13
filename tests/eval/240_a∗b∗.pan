import axioms

f240 : {s:string|?} -> unit
f240 = \s:string.
  let n = length s in
  rec w1 : {i:int|?} -> int = \i:int.
    let p1 = lt i n in
    if p1 then
      let c = charAt s i in
      let p2 = eqChar c 'a' in
      if p2 then
        let i2 = add i 1 in
        w1 i2
      else
        i
    else
      i
  in
    rec w2 : {i:int|?} -> int = \i:int.
      let p1 = lt i n in
      if p1 then
        let c = charAt s i in
        let p2 = eqChar c 'b' in
        if p2 then
          let i2 = add i 1 in
          w2 i2
        else
          i
      else
        i
    in
      let j = w1 0 in
      let k = w2 j in
      let p = eq k n in
      assert p
