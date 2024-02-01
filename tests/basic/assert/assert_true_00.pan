assert : { b:𝔹 | b = true } → 𝟙

-- NOTE: "v≠unit" reduces to "false"
f40 : {v:unit|v≠unit}
f40 = assert true

f50 : {v:unit|false}
f50 = assert true

-- NOTE:
-- these should fail because "true" is not a subtype of "false"
-- the type of "assert true" is "{v:unit|true}"
