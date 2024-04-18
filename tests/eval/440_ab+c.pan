import axioms

f440 : {s:string|?} -> unit
f440 = \s:string.
  let x = match s "ab" in
  let y = match s "c" in
  let z = or x y in
  assert z
