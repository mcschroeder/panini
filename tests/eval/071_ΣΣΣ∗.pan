import axioms

g1 : {s:string|?} -> unit
g1 = \s:string.
  let n = length s in
  let p = eq n 2 in
  assert p

g2 : {s:string|?} -> unit
g2 = \s:string.
  unit

f071 : {s:string|?} -> unit
f071 = \s:string.
  let t = slice s 0 1 in
  let _ = g1 t in
  let n = length s in
  let m = sub n 1 in
  let p = gt m 2 in
  if p then
    let t2 = slice s 2 m in
    g2 t2
  else
    unit
