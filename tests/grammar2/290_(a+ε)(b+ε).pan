import axioms

f290 : {s:string|?} -> unit
f290 = \s:string.
  let p1 = match s "" in
  let p2 = match s "a" in
  let p3 = match s "b" in
  let p4 = match s "ab" in
  let p5 = or p1 p2 in
  let p6 = or p3 p4 in
  let p7 = or p5 p6 in
  assert p7

-- NOTE: regex simplifier cannot simplify this one yet, 
-- but the grammar in the output file is equivalent to a?b?
