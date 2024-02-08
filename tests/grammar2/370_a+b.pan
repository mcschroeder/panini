import axioms

f370 : {s:string|?} -> unit
f370 = \s:string.
  let p1 = match s "a" in
  let p2 = match s "b" in
  let p3 = or p1 p2 in
  assert p3

f371 : {s:string|?} -> unit
f371 = \s:string.
  let p1 = match s "a" in
  if p1 then
    unit
  else
    let p2 = match s "b" in
    if p2 then
      unit
    else
      assert false

f372 : {s:string|?} -> unit
f372 = \s:string.
  let c = charAt s 0 in
  let p1 = match c "a" in
  let p2 = match c "b" in
  let p3 = or p1 p2 in
  let n = length s in
  let p4 = eq n 1 in
  let p5 = and p3 p4 in
  assert p5
