import axioms

f : {s:𝕊 | s ∈ } → 𝟙


-- (a+b)∗aa(a+b)∗
f460 : {s:string|?} -> unit
f460 = \s:string.

-- (b+ba)∗
f470 : {s:string|?} -> unit
f470 = \s:string.

-- (a+ε)(b+ba)∗
f480 : {s:string|?} -> unit
f480 = \s:string.

-- (a+b)∗abb
f490 : {s:string|?} -> unit
f490 = \s:string.

-- a∗b∗c∗
f500 : {s:string|?} -> unit
f500 = \s:string.
