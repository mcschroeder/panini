f60 : {v:int|?} -> {w:int|w=1}
f60 = \x:int. x

f61 : {v:int|?} -> {w:int|w=1 /\ w=v}
f61 = \x:int. x

f62 : {v:int|?} -> {w:int|w>0}
f62 = \x:int. x
