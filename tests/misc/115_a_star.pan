assert : { b:𝔹 | b = true } → 𝟙
equals : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }
length : (s:𝕊) → { n:ℤ | n ≥ 0 ∧ n = |s| }
charAt : (s:𝕊) → { i:ℤ | i ≥ 0 ∧ i < |s| } → { t:char | t = s[i] }
match  : (s:char) → (t:char) → { b:𝔹 | b = true ⟺ s = t }
add : (a:int) -> (b:int) -> { c:int | c = a + b }
lt : (a:int) -> (b:int) -> { c:bool | c = true <=> a < b }

f = λs:𝕊.
  let n = length s in
  rec w : {i:int|?} -> unit = \i:int.
    let p = lt i n in
    if p then
      let x = charAt s i in
      let p2 = match x 'a' in
      let _ = assert p2 in
      let i2 = add i 1 in
      w i2
    else
      unit
  in
    w 0
