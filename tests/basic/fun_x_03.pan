f21 : {v:int|v=1} -> int
f21 = \x:int. x

f50 : {v:int|v=1} -> {w:int|true}
f50 = \x:int. x

f51 : {v:int|v=1} -> {w:int|w=v}
f51 = \x:int. x

f52 : {v:int|v=1} -> {w:int|w=1}
f52 = \x:int. x

f72 : {v:int|?} -> {w:int|w=1}
f72 = \x:int. x

f53 : {v:int|v=1} -> {w:int|w=1 /\ w=v}
f53 = \x:int. x

f55 : {v:int|v=1} -> {w:int|?}
f55 = \x:int. x

f73 : {v:int|?} -> {w:int|w=1 /\ w=v}
f73 = \x:int. x
