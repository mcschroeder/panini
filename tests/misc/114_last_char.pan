assert : { b:𝔹 | b = true } → 𝟙
equals : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
length : (s:𝕊) → { n:ℤ | n ≥ 0 ∧ n = |s| }
charAt : (s:𝕊) → { i:ℤ | i ≥ 0 ∧ i < |s| } → { t:char | t = s[i] }
match  : (s:char) → (t:char) → { b:𝔹 | b = true ⟺ s = t }
sub : (a:int) -> (b:int) -> { c:int | c = a - b}

f = \s:string.
  let n1 = length s in
  let n2 = sub n1 1 in
  let x = charAt s n2 in
  let p = match x 'a' in
  assert p
