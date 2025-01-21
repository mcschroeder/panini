assert : { b:𝔹 | b = true } → 𝟙
match : (s:𝕊) → (t:𝕊) → {b:𝔹 | b = true ⟺ s = t}
concat : a:string -> b:string -> {c:string | c = a ++ b}

all : (s:string) -> (t:string) -> {b:bool| b = true <=> t \in re.*(s)}


f : {s:string|?} -> unit
f = \s:string.
  let p = all "hey" s in
  assert p
