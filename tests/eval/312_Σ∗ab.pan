import axioms

f312 : {s:string|?} -> unit
f312 = \s:string.
  let n = length s in
  let n1 = sub n 1 in
  let n2 = sub n 2 in
  let t = slice s n2 n1 in
  let p = match t "ab" in
  assert p
