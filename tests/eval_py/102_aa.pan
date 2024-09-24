add : (a:ℤ) → (b:ℤ) → {c:ℤ | c = a + b}
assert : {b:𝔹 | b = true} → 𝟙
eq : (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⇔ a = b}
length : (s:𝕊) → {n:ℤ | n = |s|}
lt : (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⇔ a < b}
match : (s:𝕊) → (t:𝕊) → {b:𝔹 | b = true ⇔ s = t}
slice1 : (s:𝕊) → {i:ℤ | i ≥ 0 ∧ i < |s|} → {t:𝕊 | t = s[i..i]}
f102 : {s:𝕊 | ?} → 𝟙
f102 = λs:𝕊.
  rec L2 : {i:ℤ | i = 0 \/ i = 1 \/ i = 2} → 𝟙 = λi:ℤ.
    let v0 = length s in
    let v1 = lt i v0 in
    if v1 then
      let v2 = slice1 s i in
      let v3 = match v2 "a" in
      let _ = assert v3 in
      let i = add i 1 in
      L2 i
    else
      let v4 = eq i 2 in
      let _ = assert v4 in
      unit
  in
    L2 0