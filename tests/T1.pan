assert : { b:𝔹 | b = true } → 𝟙
equals : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
length : (s:𝕊) → { n:ℤ | n ≥ 0 ∧ n = |s| }
charAt : (s:𝕊) → { i:ℤ | i ≥ 0 ∧ i < |s| } → { t:𝕊 | t = s[i] }
match  : (s:𝕊) → (t:𝕊) → { b:𝔹 | b = true ⟺ s = t }

f : 𝕊 → 𝟙 
= λs:𝕊.
    let x = charAt s 0 in
    let p1 = match x "a" in
    if p1 then
      let n = length s in
      let p2 = equals n 1 in
      assert p2
    else
      let y = charAt s 1 in
      let p3 = match y "b" in
      assert p3
