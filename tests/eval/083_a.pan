import axioms

f083 : {s:string|?} -> unit
f083 = \s:string.
  let t = slice s 0 0 in
  let p1 = match t "a" in
  let _ = assert p1 in
  let n = length s in
  let p2 = eq n 1 in 
  assert p2
