import axioms

f051 : {s:string|?} -> unit
f051 = \s:string.
  let n = length s in
  let p = eq n 1 in
  if p then
    unit
  else
    let _ = charAt s 0 in
    unit
