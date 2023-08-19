assert : { b:𝔹 | b = true } → 𝟙
startswith : (p:string) -> (s:string) -> {b:bool | b = true <=> s[0..|p|] = p}

prefix_check = \s:string.
  let prefix = "http" in
  let p = startswith prefix s in
  assert p
