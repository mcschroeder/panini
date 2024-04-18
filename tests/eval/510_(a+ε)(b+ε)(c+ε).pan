import axioms

f510 : {s:string|?} -> unit
f510 = \s:string.
  let p1 = match s "abc" in
  let p2 = match s "ab" in
  let p3 = match s "a" in
  let p4 = match s "ac" in
  let p5 = match s "bc" in
  let p6 = match s "b" in
  let p7 = match s "c" in
  let p8 = match s "" in
  let q1 = or p1 p2 in
  let q2 = or p3 p4 in
  let q3 = or p5 p6 in
  let q4 = or p7 p8 in
  let r1 = or q1 q2 in
  let r2 = or q3 q4 in
  let t1 = or r1 r2 in
  assert t1


-- NOTE: regex simplifier cannot simplify this one yet, 
-- but the grammar in the output file is equivalent to a?b?c?
