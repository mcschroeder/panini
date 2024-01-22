f3 = unit

f4 : {v:unit|?}
f4 = unit

f5 = rec f5b : {v:unit|?} = unit in f5b

f6 : {v:unit|?}
f6 = rec f6b : {v:unit|?} = unit in f6b


f7 = rec f7b : {v:int|?} = 1 in f7b


f8 = let f8b = unit in f8b

f9 : {v:unit|?}
f9 = let f9b = unit in f9b
