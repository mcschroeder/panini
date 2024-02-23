import axioms

f310 : {s:string|?} -> unit
f310 = \s:string.
  let n = length s in
  let n1 = sub n 1 in
  let c1 = charAt s n1 in
  let p1 = match c1 "b" in
  let n2 = sub n 2 in
  let c2 = charAt s n2 in
  let p2 = match c2 "a" in
  let _ = assert p1 in
  let _ = assert p2 in
  unit
