import axioms

f130 : {s:string|?} -> unit
f130 = \s:string.
  let n1 = length s in
  let n2 = sub n1 1 in
  let x = charAt s n2 in
  let p = match x "a" in
  assert p
