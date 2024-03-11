import axioms

f090 : {s:string|?} -> unit
f090 = \s:string.
  let n = length s in
  let p = eq n 0 in
  if p then 
    unit 
  else
    let c = charAt s 0 in
    let p2 = eqChar c 'a' in
    let _ = assert p2 in
    let p3 = eq n 1 in
    assert p3
