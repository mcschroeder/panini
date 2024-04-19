import axioms

f480 : {s:string|?} -> unit
f480 = \s:string.
  let n = length s in
  rec w : {i:int|?} -> unit = \i:int.
    let p = ge i n in
    if p then
      unit
    else
      let x = charAt s i in
      let p2 = eqChar x 'b' in
      let _ = assert p2 in
      let i2 = add 1 i in
      let p3 = ge i2 n in
      if p3 then
        unit
      else
        let y = charAt s i2 in
        let p4 = eqChar y 'a' in
        if p4 then
          let i3 = add 1 i2 in
          w i3
        else
          w i2
  in
    let c = charAt s 0 in
    let p0 = eqChar c 'a' in
    if p0 then
      w 1
    else
      w 0