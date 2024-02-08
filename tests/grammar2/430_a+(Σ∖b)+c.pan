import axioms

f430 : {s:string|?} -> unit
f430 = \s:string.
  let n = length s in
  let q = eq n 1 in
  let _ = assert q in
  let c = charAt s 0 in
  let pa = match c "a" in
  let pb = match c "b" in
  let pb0 = not pb in
  let pc = match c "c" in
  let p1 = or pa pb0 in
  let p2 = or p1 pc in
  assert p2

f431 : {s:string|?} -> unit
f431 = \s:string.
  let p1 = match s "a" in
  let p2 = match s "c" in
  let p = or p1 p2 in
  if p then
    unit
  else
    let n = length s in
    let q = eq n 1 in
    let _ = assert q in
    let c = charAt s 0 in
    let p = match c "b" in
    let pp = not p in
    assert pp
