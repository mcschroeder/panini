import axioms

f001 : {s:string|?} -> unit
f001 = \s:string.
  let p = match s "" in
  assert p
