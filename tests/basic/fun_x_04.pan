f32 : int -> {w:int|w=2}
f32 = \x:int. x

f33 : int -> {w:int|false}
f33 = \x:int. x

f42 : {v:int|true} -> {w:int|w=1}
f42 = \x:int. x

f43 : {v:int|true} -> {w:int|w=1 /\ w=v}
f43 = \x:int. x

f44 : {v:int|true} -> {w:int|false}
f44 = \x:int. x

f54 : {v:int|v=1} -> {w:int|false}
f54 = \x:int. x
