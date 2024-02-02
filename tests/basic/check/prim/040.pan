f40 : {v:unit|false}
f40 = unit

f41 : {v:bool|false}
f41 = true

f42 : {v:int|false}
f42 = 1

f43 : {v:string|false}
f43 = "hello"

-- NOTE: "v≠unit" reduces to "false"
f44 : {v:unit|v≠unit}
f44 = unit
