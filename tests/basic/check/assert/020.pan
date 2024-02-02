assert : { b:𝔹 | b = true } → 𝟙

f20 : unit
f20 = assert false

f21 : {v:unit|true}
f21 = assert false

f22 : {v:unit|v=unit}
f22 = assert false
