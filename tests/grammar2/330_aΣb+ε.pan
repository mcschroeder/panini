import axioms

f330 : {s:string|?} -> unit
f330 = \s:string.
  let n = length s in
  let p0 = eq n 0 in
  if p0 then
    unit
  else
    let p3 = eq n 3 in
    if p3 then
      let c0 = charAt s 0 in
      let c2 = charAt s 2 in
      let pa = match c0 "a" in
      let pb = match c2 "b" in
      let _  = assert pa in
      let _  = assert pb in
      unit
    else
      assert false

f331 : {s:string|?} -> unit
f331 = \s:string.
  let n = length s in
  let p0 = eq n 0 in
  let p3 = eq n 3 in
  let p4 = or p0 p3 in
  let _  = assert p4 in
  if p3 then
    let c0 = charAt s 0 in
    let c2 = charAt s 2 in
    let pa = match c0 "a" in
    let pb = match c2 "b" in
    let ps = and pa pb in
    assert ps
  else
    unit

