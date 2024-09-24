add : (a:ℤ) → (b:ℤ) → {c:ℤ | c = a + b}
assert : {b:𝔹 | b = true} → 𝟙
length : (s:𝕊) → {n:ℤ | n = |s|}
lt : (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⇔ a < b}
match : (s:𝕊) → (t:𝕊) → {b:𝔹 | b = true ⇔ s = t}
slice1 : (s:𝕊) → {i:ℤ | i ≥ 0 ∧ i < |s|} → {t:𝕊 | t = s[i..i]}

hex_values : {c:string|?} -> int
hex_values = \c:string.
  let p0   = match c "0" in if p0   then 0  else
  let p1   = match c "1" in if p1   then 1  else
  let p2   = match c "2" in if p2   then 2  else
  let p3   = match c "3" in if p3   then 3  else
  let p4   = match c "4" in if p4   then 4  else
  let p5   = match c "5" in if p5   then 5  else
  let p6   = match c "6" in if p6   then 6  else
  let p7   = match c "7" in if p7   then 7  else
  let p8   = match c "8" in if p8   then 8  else
  let p9   = match c "9" in if p9   then 9  else
  let p10  = match c "a" in if p10  then 10 else
  let p11  = match c "b" in if p11  then 11 else
  let p12  = match c "c" in if p12  then 12 else
  let p13  = match c "d" in if p13  then 13 else
  let p14  = match c "e" in if p14  then 14 else
  let p15  = match c "f" in if p15  then 15 else
  let p10u = match c "A" in if p10u then 10 else
  let p11u = match c "B" in if p11u then 11 else
  let p12u = match c "C" in if p12u then 12 else
  let p13u = match c "D" in if p13u then 13 else
  let p14u = match c "E" in if p14u then 14 else
  let p15u = match c "F" in if p15u then 15 else
  let _ = assert false in -1

cgi_decode : {s:𝕊 | ?} → 𝟙
cgi_decode = λs:𝕊.
  rec L1 : {i:ℤ | i >= 0 /\ i <= |s|} → 𝟙 = λi:ℤ.
    let v0 = length s in
    let v1 = lt i v0 in
    if v1 then
      let c = slice1 s i in
      let v2 = match c "+" in
      if v2 then
        let i = add i 1 in
        L1 i
      else
        let v3 = match c "%" in
        if v3 then
          let v7 = add i 1 in
          let digit_high = slice1 s v7 in
          let v6 = add i 2 in
          let digit_low = slice1 s v6 in
          let v4 = hex_values digit_high in
          let v5 = hex_values digit_low in
          let i = add i 2 in
          L1 i
        else
          let i = add i 1 in
          L1 i
    else
      unit
  in
    L1 0
