lt : (a:ℤ) → (b:ℤ) → {c:𝔹 | c = true ⟺ a < b}

-- Refines to: f : {x:ℤ | false} → {y:ℤ | true} → {z:ℤ | (x < y ⇔ z = -5) ∧ (x ≥ y ⇔ z = -3)}
-- Technically correct but not really usefull.
-- Should be something like: f : {x:ℤ | true} → {y:ℤ | true} → {z:ℤ | (x < y ⇔ z = -5) ∧ (x ≥ y ⇔ z = -3)}
f : {x:int|?} -> {y:int|?} -> {z:int| (x < y ⟺ z = -5) ∧ (x >= y ⟺ z = -3)  }
f = \(x:ℤ).\(y:ℤ).
        let p = lt x y in
        if p then
            -5
        else
            -3
