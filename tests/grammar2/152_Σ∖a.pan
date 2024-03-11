import axioms

g1 : {s:char| s = 'a'}

f152 : {s:string|?} -> unit
f152 = \s:string.
  let t = g1 in
  let c = charAt s 0 in
  let p = eqChar c t in
  let p2 = not p in
  let _ = assert p2 in
  let n = length s in
  let p3 = eq n 1 in
  assert p3
