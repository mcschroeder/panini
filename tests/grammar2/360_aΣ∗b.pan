import axioms

f360 : {s:string|?} -> unit
f360 = \s:string.
  let c0 = charAt s 0 in
  let p0 = match c0 "a" in
  let _  = assert p0 in
  let n  = length s in
  let m  = sub n 1 in
  let cn = charAt s m in
  let pn = match cn "b" in
  let _  = assert pn in
  unit
