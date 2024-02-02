assert : { b:𝔹 | b = true } → 𝟙

f10 : unit
f10 = assert true

f11 : {v:unit|true}
f11 = assert true

f12 : {v:unit|v=unit}
f12 = assert true
