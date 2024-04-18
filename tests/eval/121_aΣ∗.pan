import axioms

-- note the additional non-empty constraint on s, which leads to a recursive VC
charAt' : { s:𝕊 | 0 < |s| } → { i:ℤ | i ≥ 0 ∧ i < |s| } → { t:ℂ𝕙 | t = s[i] }

f121 : {s:string|?} -> unit
f121 = \s:string.
  let c = charAt' s 0 in
  let p = eqChar c 'a' in
  assert p
