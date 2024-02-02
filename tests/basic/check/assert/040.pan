assert : { b:𝔹 | b = true } → 𝟙

f40 : {v:unit|false}
f40 = assert false

-- NOTE: "v≠unit" reduces to "false"
f41 : {v:unit|v≠unit}
f41 = assert false

-- NOTE: 
-- these should fail because "assert false" does not typecheck.
-- the type of "assert false" is not a subtype of "{v:unit|false}"
-- the type of "assert false" does not exist!
