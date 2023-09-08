assert : { b:𝔹 | b = true } → 𝟙
length : (s:𝕊) → { n:ℤ | n ≥ 0 ∧ n = |s| }
match  : (s:𝕊) → (t:𝕊) → { b:𝔹 | b = true ⟺ s = t }
substring : (s:string) -> (i:int) -> (j:int) -> {t:string | t = s[i..j] }
sub : (a:int) -> (b:int) -> { c:int | c = a - b }

prefix_check = \s:string.
  let prefix = "http" in
  let n1 = length prefix in
  let n2 = sub n1 1 in
  let t = substring s 0 n2 in
  let p = match t prefix in
  assert p
