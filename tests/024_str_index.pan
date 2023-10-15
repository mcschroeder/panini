-- Note: The Python str.index function raises a ValueError if the needle is 
-- not found in the haystack. We can simulate this by ensuring that the SMT 
-- str.indexof function does not return -1.

str_index : (s:𝕊) → (t:𝕊) → (i:ℤ) → { j:ℤ | j = str_indexof(s,t,i) ∧ true ⟺ j ≥ 0}

f = \(s:string). 
  let j = str_index s "a" 0 in
  j
