import axioms

f050 : {s:string|?} -> unit
f050 = \s:string.
  let _ = charAt s 0 in
  unit
