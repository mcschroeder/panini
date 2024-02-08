import axioms

f400 : {s:string|?} -> unit
f400 = \s:string.
  let go = \t:string.    
    rec w : int -> bool = \i:int.
      let c = charAt s i in
      let pc = match c t in
      let p0 = eq i 0 in
      if p0 then
        pc
      else
        if pc then
          let i2 = sub i 1 in
          w i2
        else
          false
    in
      let n = length s in
      let m = sub n 1 in
      w m
  in
    let pa = go "a" in
    let pb = go "b" in
    let p = or pa pb in
    assert p
