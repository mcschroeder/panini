import axioms

g1 : {s:string|?} -> unit
g1 = \s:string.
  let n = length s in
  let p = eq n 1 in
  assert p

g2 : {s:string|?} -> unit
g2 = \s:string.
  unit

f052 : {s:string|?} -> unit
f052 = \s:string.
  let t = slice s 0 0 in
  let _ = g1 t in
  let n = length s in
  let m = sub n 1 in
  let t2 = slice s 1 m in
  let _ = g2 t2 in
  unit
