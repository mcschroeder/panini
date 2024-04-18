import axioms

f092 : {s:string|?} -> unit
f092 = \s:string.
  let p1 = match s "" in
  if p1 then
    unit
  else
    let p2 = match s "a" in
    assert p2
