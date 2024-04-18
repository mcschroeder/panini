import axioms

f431 : {s:string|?} -> unit
f431 = \s:string.
  let p1 = match s "a" in
  let p2 = match s "c" in
  let p = or p1 p2 in
  if p then
    unit
  else
    let n = length s in
    let q = eq n 1 in
    let _ = assert q in
    let c = charAt s 0 in
    let p = eqChar c 'b' in
    let pp = not p in
    assert pp
