import axioms

f280 : {s:string|?} -> unit
f280 = \s:string.
  let p1 = match s "ab" in
  let p2 = match s "" in
  let p3 = or p1 p2 in
  assert p3

f281 : {s:string|?} -> unit
f281 = \s:string.
  let n = length s in
  let p0 = eq n 0 in
  if p0 then
    unit
  else
    let p1 = match s "ab" in
    assert p1
