assert : { b:𝔹 | b = true } → 𝟙
match : (s:𝕊) → (t:𝕊) → {b:𝔹 | b = true ⟺ s = t}
concat : a:string -> b:string -> {c:string | c = a ++ b}

f : {s:string|?} -> unit
f = \s:string.
  let x = "hello" in
  let y = "world" in
  let z1 = concat x " " in
  let z2 = concat z1 y in
  let p = match s z2 in
  assert p
