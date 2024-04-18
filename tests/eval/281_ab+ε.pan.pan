import axioms

f281 : {s:string|?} -> unit
f281 = \s:string.
  let n = length s in
  let p0 = eq n 0 in
  if p0 then
    unit
  else
    let p1 = match s "ab" in
    assert p1
