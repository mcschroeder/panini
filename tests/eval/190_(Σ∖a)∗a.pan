import axioms

f190 : {s:string|?} -> unit
f190 = \s:string.
  let n = length s in
  let m = sub n 1 in
  let i = index s 'a' in
  let p = eq i m in
  let _ = assert p in
  let q = ge i 0 in
  let _ = assert q in
  unit
