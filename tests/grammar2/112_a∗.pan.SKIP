import axioms

loop : s:string -> ({i:int|i ≥ 0 ∧ i < |s|} -> unit) -> unit

f112 : {s:string|?} -> unit
f112 = \s:string.
  let w = \i:int.
    let c = charAt s i in
    let p = match c "a" in
    assert p
  in
    loop s w
