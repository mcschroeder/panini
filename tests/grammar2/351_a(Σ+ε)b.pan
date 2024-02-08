import axioms

f351 : {s:string|?} -> unit
f351 = \s:string.
  let c0 = charAt s 0 in
  let p0 = match c0 "a" in
  let _  = assert p0 in
  let n  = length s in
  let pn = le n 3 in
  let _  = assert pn in
  let m  = sub n 1 in
  let cm = charAt s m in
  let pm = match cm "b" in
  let _  = assert pm in
  unit
