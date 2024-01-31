f00 = \x:int. x

f80 : {v:int|?} -> {w:int|?}
f80 = \x:int. x

f34 : int -> {w:int|?}
f34 = \x:int. x

f41 : {v:int|true} -> {w:int|w=v}
f41 = \x:int. x

f45 : {v:int|true} -> {w:int|?}
f45 = \x:int. x

f71 : {v:int|?} -> {w:int|w=v}
f71 = \x:int. x
