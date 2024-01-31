assert : { b:𝔹 | b = true } → 𝟙

f01 = assert false

f11 : unit
f11 = assert false

f21 : {v:unit|true}
f21 = assert false

f31 : {v:unit|v=unit}
f31 = assert false

f61 : {v:unit|?}
f61 = assert false
