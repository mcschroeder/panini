add : (a:ℤ) → (b:ℤ) → {c:ℤ | c = a + b}
and : (a:𝔹) → (b:𝔹) → {c:𝔹 | c = true ⇔ a = true ∧ b = true}
assert : {b:𝔹 | b = true} → 𝟙
length : (s:𝕊) → {n:ℤ | n = |s|}
lt : (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⇔ a < b}
match : (s:𝕊) → (t:𝕊) → {b:𝔹 | b = true ⇔ s = t}
slice1 : (s:𝕊) → {i:ℤ | i ≥ 0 ∧ i < |s|} → {t:𝕊 | t = s[i..i]}
f540 : {s:𝕊 | ?} → 𝟙
f540 = λs:𝕊.
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
        let v7 = add i 2 in
        let v8 = slice1 s v7 in
        let v9 = match v8 "b" in
        let v10 = and v6 v9 in
        if v10 then
          let i = add i 3 in
          L1 i
        else
          assert false
      else
        let i = add i 1 in
        L1 i
    else
      unit
  in
    L1 0