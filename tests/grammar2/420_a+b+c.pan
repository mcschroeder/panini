import axioms

f420 : {s:string|?} -> unit
f420 = \s:string.
  let p1 = match s "a" in 
  let p2 = match s "b" in 
  let p3 = match s "c" in
  let p4 = or p1 p2 in
  let p5 = or p3 p4 in
  assert p5

f421 : {s:string|?} -> unit
f421 = \s:string.
  let c = charAt s 0 in
  let n = length s in
  let q = eq n 1 in
  let _ = assert q in
  let p1 = match c "a" in
  if p1 then
    unit
  else
    let p2 = match c "b" in
    if p2 then
      unit
    else
      let p3 = match c "c" in
      if p3 then
        unit
      else
        assert false
