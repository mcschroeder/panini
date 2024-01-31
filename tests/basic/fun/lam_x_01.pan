f10 : int -> int
f10 = \x:int. x

f20 : {v:int|true} -> int
f20 = \x:int. x

f23 : {v:int|?} -> int
f23 = \x:int. x

f30 : int -> {w:int|true}
f30 = \x:int. x

f40 : {v:int|true} -> {w:int|true}
f40 = \x:int. x

f70 : {v:int|?} -> {w:int|true}
f70 = \x:int. x
