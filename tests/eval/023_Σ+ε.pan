import axioms

f023 : {s:string|?} -> unit
f023 = \s:string.
  let n = length s in
  let p1 = eq n 0 in
  if p1 then
    unit
  else
    let _ = charAt s 0 in
    let p2 = eq n 1 in
    if p2 then
      unit
    else
      assert false
