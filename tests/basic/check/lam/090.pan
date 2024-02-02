f90 : {v:int|true} -> {w:int|w=v}
f90 = \x:int. x

f91 : {v:int|v=1} -> {w:int|w=v}
f91 = \x:int. x

f92 : {v:int|v=1} -> {w:int|w=1}
f92 = \x:int. x

f93 : {v:int|v=1} -> {w:int|w=1 /\ w=v}
f93 = \x:int. x
