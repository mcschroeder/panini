import axioms

f210 : {s:string|?} -> unit
f210 = \s:string.
  let p = match s "ab" in
  assert p
