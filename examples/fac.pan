let fac = \x.
  let L1 = \r0. \x0.
    if x0 
    then
      let r1 = mul r0 x0 in 
      let x1 = sub x0 1 in 
      L1 r1 x1
    else 
      r0
  in
    let r = 1 in 
    L1 r x
in 
  fac 10
