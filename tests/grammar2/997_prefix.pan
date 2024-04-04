import axioms

find : (s:𝕊) → (c:ℂ𝕙) → {i:ℤ | i = index(s,c)}

is_email : {email:string|?} -> unit
is_email = \email:string.
   let at = find email '@' in
   let p = gt at 0 in
   assert p
