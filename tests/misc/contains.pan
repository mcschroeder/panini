assert : { b:𝔹 | b = true } → 𝟙
match : (s:𝕊) → (t:𝕊) → {b:𝔹 | b = true ⟺ s = t}
concat : a:string -> b:string -> {c:string | c = a ++ b}
all : (s:string) -> (t:string) -> {b:bool| b = true <=> t \in re_star(s)}

contains : (s:string) -> (t:string) -> {b:bool| b = str_contains(s,t)}

f : {s:string|?} -> unit
f = \s:string.
  let p = contains s "hey" in
  assert p
