import axioms

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
