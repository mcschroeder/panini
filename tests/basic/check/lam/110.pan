f110 : int -> {w:int|w=2}
f110 = \x:int. x

f111 : int -> {w:int|false}
f111 = \x:int. x

f112 : {v:int|true} -> {w:int|w=1}
f112 = \x:int. x

f113 : {v:int|true} -> {w:int|w=1 /\ w=v}
f113 = \x:int. x

f114 : {v:int|true} -> {w:int|false}
f114 = \x:int. x

f115 : {v:int|v=1} -> {w:int|false}
f115 = \x:int. x
