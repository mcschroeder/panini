import axioms

f024 : {s:string|?} -> unit
f024 = \s:string.
  let n = length s in
  let x3 = eq n 3 in
  let x2 = eq n 2 in
  let x1 = eq n 1 in
  let x0 = eq n 0 in
  if x1 then
    unit
  else
    if x0 then
      unit
    else
      assert false
