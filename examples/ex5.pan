charAt : (s:𝕊) → { i:ℤ | i ≥ 0 ∧ i < |s| } → { t:𝕊 | t = s[i] }
match  : (s:𝕊) → (t:𝕊) → { b:𝔹 | b = true ⟺ s = t }

add : (a:ℤ) → (b:ℤ) → { c:ℤ | c = a + b }
eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

assert : { b:𝔹 | b = true } → 𝟙

parser : 𝕊 → 𝟙
= \s:𝕊.
    rec go : ℤ → 𝟙 = λi1:ℤ.
      let x = charAt s i1 in
      let p1 = match x "a" in
      if p1 then
        let i2 = add i1 1 in 
        go i2
      else
        let p2 = eq i1 5 in
        assert p2
    in
      go 0
