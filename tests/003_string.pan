f0 = "hello"

f1 : string
f1 = "hello"

f2 : {v:string|?}
f2 = "hello"

f3 : {v:string|v="hello"}
f3 = "hello"

f4 : {v:string|v="goodbye"}   -- expected error
f4 = "hello"

f5 : unit       -- expected error
f5 = "hello"

f6 : {v:unit|?}   -- expected error
f6 = "hello"

f7 : bool       -- expected error
f7 = "hello"

f8 : {v:bool|?}       -- expected error
f8 = "hello"

f9 : int        -- expected error
f9 = "hello"

f10 : {v:int|?}       -- expected error
f10 = "hello"
