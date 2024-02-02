assert : { b:𝔹 | b = true } → 𝟙

f60 : {x:bool|?} -> {v:unit|false}
f60 = \x:bool. assert x

-- NOTE:
-- the precondition should be inferred as "false"
-- because "x = false" leads to "assert false" which would not type check
-- so the only way to call f60 correctly is to not call it at all!
