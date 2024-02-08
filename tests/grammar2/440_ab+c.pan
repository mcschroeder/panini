import axioms

f440 : {s:string|?} -> unit
f440 = \s:string.
  let x = match s "ab" in
  let y = match s "c" in
  let z = or x y in
  assert z

f441 : {s:string|?} -> unit
f441 = \s:string.
  let c0 = charAt s 0 in
  let p0 = match c0 "a" in
  if p0 then
    let c1 = charAt s 1 in
    let p1 = match c1 "b" in
    if p1 then
      let n = length s in
      let q = eq n 2 in
      assert q
    else
      assert false
  else
    let p2 = match c0 "c" in
    assert p2
