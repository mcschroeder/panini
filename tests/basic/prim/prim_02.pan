
a4 : {v:unit|false}
a4 = unit

b4 : {v:bool|false}
b4 = true

c4 : {v:int|false}
c4 = 1

d4 : {v:string|false}
d4 = "hello"

----------------------------------------

-- NOTE: "v≠unit" reduces to "false"
a8 : {v:unit|v≠unit}
a8 = unit



-- NOTE:
-- all of these should fail because "true" is not a subtype of "false"
