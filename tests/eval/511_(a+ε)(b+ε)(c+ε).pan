import axioms

f511 : {s:string|?} -> unit
f511 = \s:string.
  let p1 = match s "abc" in
  let p2 = match s "ab" in
  let p3 = match s "a" in
  let p4 = match s "ac" in
  let p5 = match s "bc" in
  let p6 = match s "b" in
  let p7 = match s "c" in
  let p8 = match s "" in
  let q1 = or p1 p2 in
  let q2 = or q1 p3 in
  let q3 = or q2 p4 in
  let q4 = or q3 p5 in
  let q5 = or q4 p6 in
  let q6 = or q5 p7 in
  let q7 = or q6 p8 in
  assert q7

-- NOTE: regex simplifier cannot simplify this one yet, 
-- but the grammar in the output file is equivalent to a?b?c?
