assert : { b:𝔹 | b = true } → 𝟙

f30 : {v:unit|false}
f30 = assert true

-- NOTE: "v≠unit" reduces to "false"
f31 : {v:unit|v≠unit}
f31 = assert true

-- NOTE:
-- these should fail because "true" is not a subtype of "false"
-- the type of "assert true" is "{v:unit|true}"
