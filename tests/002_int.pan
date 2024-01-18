f0 = 1

f1 : int
f1 = 1

f2 : {v:int|?}
f2 = 1

f3 : {v:int|v=1}
f3 = 1

f4 : {v:int|v>=1}
f4 = 1

f5 : {v:int|v<2}
f5 = 1

f6 : {v:int|v≠2}
f6 = 1

f7 : {v:int|v=2}    -- expected error
f7 = 1

f8 : {v:int|v>2}    -- expected error
f8 = 1

f9 : {v:int|v<1}    -- expected error
f9 = 1

f10 : {v:int|v≠1}   -- expected error
f10 = 1

f11 : unit    -- expected error
f11 = 1

f12 : {v:unit|?}    -- expected error
f12 = 1

f13 : bool    -- expected error
f13 = 1

f14 : {v:bool|?}    -- expected error
f14 = 1

f15 : string    -- expected error
f15 = 1

f16 : {v:string|?}    -- expected error
f16 = 1
