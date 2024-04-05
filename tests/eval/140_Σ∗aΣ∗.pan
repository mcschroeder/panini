import axioms

f140 : {s:string|?} -> unit
f140 = \s:string.
  let i = index s 'a' in
  let p = ge i 0 in
  assert p

-- NOTE: the gramamr [^a]*a.* is equivalent to .*a.*
