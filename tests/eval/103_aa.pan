import axioms

f103 : {s:string|?} -> unit
f103 = \s:string.
  let t = "aa" in
  let n = length t in
  let m = length s in
  let p0 = eq n m in
  let _ = assert p0 in
  let i1 = 0 in
  let c1 = charAt t i1 in
  let d1 = charAt s i1 in
  let p1 = eqChar c1 d1 in
  let _ = assert p1 in
  let i2 = add i1 1 in
  let c2 = charAt t i2 in
  let d2 = charAt s i2 in
  let p2 = eqChar c2 d2 in
  let _ = assert p2 in
  unit
