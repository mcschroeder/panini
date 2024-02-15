import axioms

f115 : {s:string|?} -> unit
f115 = \s0:string.  
  rec w : string -> int -> unit = \s:string. \i:int.
    let n = length s in
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = match x "a" in
      let _ = assert p2 in
      let i2 = add i 1 in
      w s i2
    else
      unit
  in
    w s0 0
