assert : { b:𝔹 | b = true } → 𝟙

f20 = assert false

f21 : {v:unit|?}
f21 = assert false
