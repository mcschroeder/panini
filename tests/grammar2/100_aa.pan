import axioms

f100 : {s:string|?} -> unit
f100 = \s:string.
  let p = match s "aa" in
  assert p

f101 : {s:string|?} -> unit
f101 = \s:string.
  let c1 = charAt s 0 in
  let c2 = charAt s 1 in
  let p1 = match c1 "a" in
  let p2 = match c2 "a" in
  let _ = assert p1 in
  let _ = assert p2 in
  let n = length s in
  let p3 = eq n 2 in
  assert p3
