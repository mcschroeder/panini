import axioms

f080 : {s:string|?} -> unit
f080 = \s:string.
  let p = match s "a" in
  assert p
