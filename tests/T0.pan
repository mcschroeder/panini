assert : { b:𝔹 | b = true } → 𝟙
equals : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f : ℤ → 𝟙
= λx:ℤ.
    let p = equals x 1 in
    assert p
