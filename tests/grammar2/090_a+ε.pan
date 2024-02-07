import axioms

f090 : {s:string|?} -> unit
f090 = \s:string.
  let n = length s in
  let p = eq n 0 in
  if p then 
    unit 
  else
    let c = charAt s 0 in
    let p2 = match c "a" in
    let _ = assert p2 in
    let p3 = eq n 1 in
    assert p3

f091 : {s:string|?} -> unit
f091 = \s:string.
  let n = length s in
  let p1 = gt n 1 in
  let p2 = not p1 in
  let _ = assert p2 in
  let p3 = eq n 0 in
  if p3 then
    unit
  else
    let c = charAt s 0 in
    let p4 = match c "a" in
    assert p4

f092 : {s:string|?} -> unit
f092 = \s:string.
  let p1 = match s "" in
  if p1 then
    unit
  else
    let p2 = match s "a" in
    assert p2

f093 : {s:string|?} -> unit
f093 = \s:string.
  let p1 = match s "a" in
  let n = length s in
  let p2 = eq n 0 in
  let p3 = or p1 p2 in
  assert p3
