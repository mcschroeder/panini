import axioms

f140 : {s:string|?} -> unit
f140 = \s:string.
  let _ = index s "a" 0 in
  unit
