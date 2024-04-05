import axioms

g1 : {s:string| s = "a"}

f151 : {s:string|?} -> unit
f151 = \s:string.
  let t = g1 in
  let p = match s t in
  let p2 = not p in
  let _ = assert p2 in
  let n = length s in
  let m = length t in
  let p3 = eq n m in
  assert p3
