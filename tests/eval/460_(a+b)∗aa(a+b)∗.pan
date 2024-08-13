import axioms

f460 : {s:string|?} -> unit
f460 = \s:string.
  rec w : {i:int|?} -> int = \j:int.
    let n = length s in
    let q1 = ge j n  in
    if q1 then
      j
    else
      let z = charAt s j    in
      let q2 = eqChar z 'a' in
      let q3 = eqChar z 'b' in
      let q4 = or q2 q3     in
      let _  = assert q4    in
      let j2 = add j 1      in
      w j2
  in
    let i0 = 0            in
    let i1 = w i0         in
    let x  = charAt s i1  in
    let p1 = eqChar x 'a' in
    let _  = assert p1    in
    let i2 = add i1 1     in
    let y  = charAt s i2  in
    let p2 = eqChar y 'a' in
    let _  = assert p2    in
    let i3 = add i2 1     in
    let i4 = w i3         in
    let n  = length s     in
    let p3 = eq i4 n      in
    assert p3
