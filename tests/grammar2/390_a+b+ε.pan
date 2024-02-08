import axioms

f390 : {s:string|?} -> unit
f390 = \s:string.
  let p1 = match s "a" in
  let p2 = match s "b" in
  let p3 = match s "" in
  let p4 = or p1 p2 in
  let p5 = or p3 p4 in
  assert p5

f391 : {s:string|?} -> unit
f391 = \s:string.
  let n = length s in
  let p0 = eq n 0 in
  if p0 then
    unit
  else
    let p1 = eq n 1 in
    let _ = assert p1 in
    let c0 = charAt s 0 in
    let pa = match c0 "a" in
    if pa then
      unit
    else
      let pb = match c0 "b" in
      if pb then
        unit
      else
        assert false
