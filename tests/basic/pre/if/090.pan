assert : { b:𝔹 | b = true } → 𝟙
not : a:bool -> {b:bool|b=¬a}

f90 : {x:bool|?} -> unit
f90 = \x:bool. 
  if x then 
    assert x 
  else 
    assert x

f91 : {x:bool|?} -> unit
f91 = \x:bool. 
  let y = not x in
  if x then 
    assert y
  else 
    assert x

f92 : {x:bool|?} -> unit
f92 = \x:bool. 
  let y = not x in
  if x then 
    assert y 
  else 
    assert y

f93 : {x:bool|?} -> unit
f93 = \x:bool. 
  let y = not x in
  if x then 
    assert x
  else 
    assert y
