f0 = unit

f1 : unit
f1 = unit

f2 : {v:unit|true}
f2 = unit

f3 : {v:unit|false}
f3 = unit

f4 : {v:unit|?}
f4 = unit

f5 : int      -- expected error
f5 = unit

f6 : bool     -- expected error
f6 = unit

f7 : string   -- expected error
f7 = unit

f8 : {v:int|?}      -- expected error
f8 = unit

f9 : {v:bool|?}     -- expected error
f9 = unit

f10 : {v:string|?}   -- expected error
f10 = unit

f11 : {v:unit|v=unit}
f11 = unit
