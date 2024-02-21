import axioms

f020 : {s:string|?} -> unit
f020 = \s:string.
  let n = length s in
  let p = le n 1 in
  assert p

f021 : {s:string|?} -> unit
f021 = \s:string.
  let n = length s in
  let p1 = eq n 0 in
  let p2 = eq n 1 in
  let p3 = or p1 p2 in
  assert p3

f022 : {s:string|?} -> unit
f022 = \s:string.
  let n = length s in
  let p1 = eq n 0 in
  if p1 then
    unit
  else
    let p2 = eq n 1 in
    if p2 then
      unit
    else
      assert false
