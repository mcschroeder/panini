add : (a:ℤ) → (b:ℤ) → {c:ℤ | c = a + b}
assert : {b:𝔹 | b = true} → 𝟙
length : (s:𝕊) → {n:ℤ | n = |s|}
lt : (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⇔ a < b}
match : (s:𝕊) → (t:𝕊) → {b:𝔹 | b = true ⇔ s = t}
slice1 : (s:𝕊) → {i:ℤ | i ≥ 0 ∧ i < |s|} → {t:𝕊 | t = s[i..i]}
f530 : {s:𝕊 | ?} → 𝟙
f530 = λs:𝕊.
  rec L1 : {i:ℤ | i >= 0 /\ i <= |s|} → 𝟙 = λi:ℤ.
    let v0 = length s in
    let v1 = lt i v0 in
    if v1 then
      let v2 = slice1 s i in
      let v3 = match v2 "a" in
      if v3 then
        let v4 = add i 1 in
        let v5 = slice1 s v4 in
        let v6 = match v5 "b" in
        let _ = assert v6 in
        let i = add i 2 in
        L1 i
      else
        let i = add i 1 in
        L1 i
    else
      unit
  in
    L1 0