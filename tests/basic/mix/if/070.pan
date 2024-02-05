assert : { b:𝔹 | b = true } → 𝟙

f70 : {x:bool|?} -> {v:int|?}
f70 = \x:bool. 
  if x then 
    let _ = assert false in
    1
  else
    2

f71 : {x:bool|?} -> {v:int|?}
f71 = \x:bool. 
  if x then 
    let _ = assert true in
    1
  else
    2
