assert : { b:𝔹 | b = true } → 𝟙

f00 = assert true

f10 : unit
f10 = assert true

f20 : {v:unit|true}
f20 = assert true

f30 : {v:unit|v=unit}
f30 = assert true

f60 : {v:unit|?}
f60 = assert true
