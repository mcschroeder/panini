import axioms

f100 : {s:string|?} -> unit
f100 = \s:string.
  let p = match s "aa" in
  assert p
