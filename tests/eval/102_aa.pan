import axioms

f102 : {s:string|?} -> unit
f102 = \s:string.
  let t = slice s 0 1 in 
  let p1 = match t "aa" in
  let _ = assert p1 in
  let n = length s in
  let p2 = eq n 2 in
  assert p2
