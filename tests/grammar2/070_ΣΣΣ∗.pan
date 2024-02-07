import axioms

f070 : {s:string|?} -> unit
f070 = \s:string.
  let _ = charAt s 0 in
  let _ = charAt s 1 in
  unit
