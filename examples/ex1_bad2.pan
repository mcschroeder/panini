import base.pan

f : 𝕊 → 𝟙 
= λs:𝕊.
    let x = charAt s 0 in
    let p1 = match x 1 in
    if p1 then
      let n = length s in
      let p2 = equals n 1 in
      assert p2
    else
      let y = charAt s 1 in
      let p3 = match y "b" in
      assert p3
