import axioms

f150 : {s:string|?} -> unit
f150 = \s:string.
  let x = charAt s 0 in
  let p = match x "a" in 
  let p2 = not p in
  let _ = assert p2 in
  let n = length s in
  let p3 = n eq 1 in
  assert p3
