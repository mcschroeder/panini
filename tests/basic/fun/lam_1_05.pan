
f33 : int -> {w:int|false}
f33 = \x:int. 1

f43 : {v:int|true} -> {w:int|false}
f43 = \x:int. 1

f53 : {v:int|v=1} -> {w:int|false}
f53 = \x:int. 1

f63 : {v:int|v=2} -> {w:int|false}
f63 = \x:int. 1
