assert : { b:𝔹 | b = true } → 𝟙
int_ge : (a:ℤ) → (b:ℤ) → { v:𝔹 | v = true ⟺ a ≥ b }
int_le : (a:ℤ) → (b:ℤ) → { v:𝔹 | v = true ⟺ a ≤ b }
int_eq : (a:ℤ) → (b:ℤ) → { v:𝔹 | v = true ⟺ a = b }
int_add : (a:ℤ) → (b:ℤ) → { c:ℤ | c = a + b }
int_sub : (a:ℤ) → (b:ℤ) → { c:ℤ | c = a - b }

-- returns index of first occurrence of t in s, beginning at i
-- returns -1 if t not found
str_find : (s:𝕊) → (t:𝕊) → (i:ℤ) → { j:ℤ | j = str_indexof(s,t,i) }

-- returns index of first occurrence of t in s, beginning at i
-- asserts that t is found
str_index : (s:𝕊) → (t:𝕊) → (i:ℤ) → { j:ℤ | j = str_indexof(s,t,i) /\ j >= 0 }

is_valid_cidr = \(string_network:string).
--  let i1 = str_find string_network "b" 0 in
--  let p1 = int_ge i1 0 in
--  let _ = assert p1 in
--  let i2 = int_add i1 1 in
--  let i3 = str_find string_network "b" i2 in
--  let p2 = int_eq i3 -1 in
--  let _ = assert p2 in

  let slash_index = str_index string_network "/" 0 in

--  let i1 = int_add slash_index 1 in
--  let i2 = str_length slash_index in
--  let i3 = int_sub i2 1 in
--  let s1 = str_sub string_network i1 i3 in
--  let mask = str_to_int s1 in
--  let p2 = int_ge mask 1 in
--  let p3 = int_le mask 32 in
--  let p4 = and p2 p3 in
--  let _ = assert p4 in
--  let i4 = int_sub slash_index 1 in
--  let s2 = str_sub string_network 0 i4 in
--  let _ = inet_aton s2 in

  unit
