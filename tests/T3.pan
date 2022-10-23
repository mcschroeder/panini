assert : { b:𝔹 | b = true } → 𝟙
equals : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
charAt : (s:𝕊) → { i:ℤ | i ≥ 0 ∧ i < |s| } → { t:𝕊 | t = s[i] }
match  : (s:𝕊) → (t:𝕊) → { b:𝔹 | b = true ⟺ s = t }

f : 𝕊 → 𝟙 
= λs:𝕊.
    let x = charAt s 0 in
    let p = match x "a" in
    assert p
