import axioms

f350 : {s:string|?} -> unit
f350 = \s:string.
  let p1 = match s "ab" in
  if p1 then
    unit
  else
    let c0 = charAt s 0 in
    let c2 = charAt s 2 in
    let p0 = match c0 "a" in
    let p2 = match c2 "b" in
    let _  = assert p0 in
    let _  = assert p2 in
    let n  = length s in
    let q  = eq n 3 in 
    assert q
