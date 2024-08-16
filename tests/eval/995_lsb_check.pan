import axioms

lsb_check : {s:𝕊 | ?} → 𝟙
lsb_check = λs:𝕊.
  rec L2 : {i:ℤ | i >= 0 /\ i <= |s|-1} → 𝟙 = λi:ℤ.
    let v0 = length s in
    let v1 = sub v0 1 in
    let v2 = lt i v1 in
    if v2 then
      let v3 = slice1 s i in
      let v4 = match v3 "0" in
      let _ = assert v4 in
      let i = add i 1 in
      L2 i
    else
      let v5 = slice1 s i in
      let v6 = match v5 "1" in
      let _ = assert v6 in
      unit
  in
    L2 0