f10 : int -> int
f10 = \x:int. 1

f20 : {v:int|true} -> int
f20 = \x:int. 1

f24 : {v:int|?} -> int
f24 = \x:int. 1

f30 : int -> {w:int|true}
f30 = \x:int. 1

f40 : {v:int|true} -> {w:int|true}
f40 = \x:int. 1

f80 : {v:int|?} -> {w:int|true}
f80 = \x:int. 1
