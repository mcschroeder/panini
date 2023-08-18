assert : { b:𝔹 | b = true } → 𝟙
startswith : (p:string) -> {s:string | s[0..|p|] = p } -> bool

prefix_check = \s:string.
  let prefix = "http" in
  let p = startswith prefix s in
  assert p
