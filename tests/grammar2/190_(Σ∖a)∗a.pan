import axioms

f190 : {s:string|?} -> unit
f190 = \s:string.
  let n = length s in
  let m = sub n 1 in
  let i = index s 'a' 0 in
  let p = eq i m in
  assert p
