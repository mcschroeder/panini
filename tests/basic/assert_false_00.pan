assert : { b:𝔹 | b = true } → 𝟙

-- NOTE: "v≠unit" reduces to "false"
f41 : {v:unit|v≠unit}
f41 = assert false

f51 : {v:unit|false}
f51 = assert false
