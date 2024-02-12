import axioms

f114 : {s:string|?} -> unit
f114 = \s0:string.  
  rec w : int -> string -> unit = \i:int. \s:string.
    let n = length s in
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = match x "a" in
      let _ = assert p2 in
      let i2 = add i 1 in
      w i2 s
    else
      unit
  in
    w 0 s0
