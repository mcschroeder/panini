import axioms

f513 : {s:string|?} -> unit
f513 = \s:string.
  let n = length s in
  let p3 = eq n 3 in
  let p2 = eq n 2 in
  let p1 = eq n 1 in
  let p0 = eq n 0 in
  if p3 then
    let c0 = charAt s 0 in
    let c1 = charAt s 1 in
    let c2 = charAt s 2 in
    let q0 = match c0 "a" in
    let _  = assert q0 in 
    let q1 = match c2 "b" in
    let _  = assert q1 in 
    let q2 = match c2 "c" in
    let _  = assert q2 in 
    unit
  else
    if p2 then
      let c0 = charAt s 0 in
      let c1 = charAt s 1 in
      let q0 = match c0 "a" in
      let q1 = match c0 "b" in
      let r1 = match c1 "b" in
      let r2 = match c1 "c" in
      if q0 then        
        if r1 then unit else 
        assert r2
      else
        if q1 then
          if r2 then unit else
          assert false
        else
          assert false
    else
      if p1 then
        let c0 = charAt s 0 in
        let q0 = match c0 "a" in
        let q1 = match c0 "b" in
        let q2 = match c0 "c" in
        if q0 then unit else 
        if q1 then unit else
        if q2 then unit else
        assert false
      else
        if p0 then unit else
        assert false
