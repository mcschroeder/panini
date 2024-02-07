import axioms

f080 : {s:string|?} -> unit
f080 = \s:string.
  let p = match s "a" in
  assert p

f081 : {s:string|?} -> unit
f081 = \s:string.
  let n = length s in
  let p = eq n 1 in
  let _ = assert p in
  let c = charAt s 0 in
  let p = match c "a" in
  assert p

f082 : {s:string|?} -> unit
f082 = \s:string.
  let n = length s in
  let p = le n 1 in
  let _ = assert p in
  let c = charAt s 0 in
  let p = match c "a" in
  assert p
