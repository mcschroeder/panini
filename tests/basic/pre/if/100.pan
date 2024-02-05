assert : { b:𝔹 | b = true } → 𝟙

f100 : {x:bool|?} -> {v:int|v=1}
f100 = \x:bool.
  if x then
    let _ = assert false in
    1
  else
    2

f101 : {x:bool|?} -> {v:int|v=1}
f101 = \x:bool.
  if x then
    let _ = assert true in
    1
  else
    2

f102 : {x:bool|?} -> {v:int|v=2}
f102 = \x:bool.
  if x then
    let _ = assert false in
    1
  else
    2

f103 : {x:bool|?} -> {v:int|v=2}
f103 = \x:bool.
  if x then
    let _ = assert true in
    1
  else
    2
