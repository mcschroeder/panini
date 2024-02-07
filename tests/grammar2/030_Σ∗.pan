import axioms

f030 : {s:string|?} -> unit
f030 = \s:string.
  unit

f031 : {s:string|?} -> unit
f031 = \s:string.
  let n = length s in
  let p = ge n 0 in
  assert p

f032 : {s:string|?} -> unit
f032 = \s:string.  
  let n = length s in
  let p = eq n 0 in
  if p then
    unit
  else
    let _ = charAt s 0 in
    unit
