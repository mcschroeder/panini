f0 = true

f1 = false

f2 : bool
f2 = true

f3 : bool
f3 = false

f4 : {v:bool|?}
f4 = true

f5 : {v:bool|?}
f5 = false

f6 : {v:bool|v=true}
f6 = true

f7 : {v:bool|v=false}
f7 = false

f8 : {v:bool|v=false}   -- expected error
f8 = true

f9 : {v:bool|v=true}    -- expected error
f9 = false

f10 : unit    -- expected error
f10 = true

f11 : unit    -- expected error
f11 = false

f12 : {v:unit|?}    -- expected error
f12 = true

f13 : {v:unit|?}    -- expected error
f13 = false

f14 : int   -- expected error
f14 = true

f15 : int   -- expected error
f15 = false

f16 : {v:int|?}   -- expected error
f16 = true

f17 : {v:int|?}   -- expected error
f17 = false

f18 : string    -- expected error
f18 = true

f19 : string    -- expected error
f19 = false

f20 : {v:string|?}    -- expected error
f20 = true

f21 : {v:string|?}    -- expected error
f21 = false