assert : { b:𝔹 | b = true } → 𝟙
equals : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> a = true /\ b = true}

f : {v:ℤ | ?} → 𝟙
f = λx:ℤ.
    let p1 = equals x 1 in
    let p2 = equals x 2 in
    let p3 = and p1 p2 in
    assert p3
