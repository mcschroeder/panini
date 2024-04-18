assert : { b:𝔹 | b = true } → 𝟙
equals : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
charAt : (s:𝕊) → { i:ℤ | i ≥ 0 ∧ i < |s| } → { t:char | t = s[i] }
match  : (s:char) → (t:char) → { b:𝔹 | b = true ⟺ s = t }

f = λs:𝕊.
    let x = charAt s 0 in
    let p = match x 'a' in
    assert p
