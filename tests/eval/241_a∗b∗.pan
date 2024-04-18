import axioms

g = \s:string. \t:char. \i0:int.
  let n = length s in
  rec g' : int -> int = \i:int.  
    let p1 = le i n in
    if p1 then
      let c = charAt s i in
      let p2 = eqChar c t in
      if p2 then
        let i2 = add i 1 in
        g' i2
      else
        i
    else
      i
  in
    g' i0

f241 : {s:string|?} -> unit
f241 = \s:string.
  let i = g s 'a' 0 in
  let j = g s 'b' i in
  let n = length s in
  let p = eq j n in
  assert p
