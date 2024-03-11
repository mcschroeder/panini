import axioms

f450 : {s:string|?} -> unit
f450 = \s:string.
  let assertPat = \t:string.
    let c0 = charAt t 0 in
    let p1 = eqChar c0 'a' in
    let _ = assert p1 in
    let c1 = charAt t 1 in
    let p2 = eqChar c1 'a' in
    let p3 = eqChar c1 'b' in
    let p4 = or p2 p3 in
    let p5 = not p4 in
    let _ = assert p5 in
    let c2 = charAt t 2 in
    let p6 = eqChar c2 'b' in
    let _ = assert p6 in
    unit
  in
    let n = length s in
    rec go : int -> unit = \i:int.
      let p = lt i n in
      if p then
        let j = add i 2 in
        let t = slice s i j in
        let _ = assertPat t in
        let i2 = add j 1 in
        go i2
      else
        unit 
    in
      go 0     