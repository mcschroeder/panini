import axioms

f010 : {s:string|?} -> unit
f010 = \s:string.
  let n = length s in
  let p = eq n 1 in
  assert p

f011 : {s:string|?} -> unit
f011 = \s:string.
  let _ = charAt s 0 in
  let n = length s in
  let p = le n 1 in
  assert p

f012 : {s:string|?} -> unit
f012 = \s:string.
  let n = length s in
  let p = le n 1 in
  let _ = assert p in
  let _ = charAt s 0 in
  unit