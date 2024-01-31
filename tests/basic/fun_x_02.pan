f22 : {v:int|false} -> int
f22 = \x:int. x

f60 : {v:int|false} -> {w:int|true}
f60 = \x:int. x

f61 : {v:int|false} -> {w:int|w=v}
f61 = \x:int. x

f62 : {v:int|false} -> {w:int|w=1}
f62 = \x:int. x

f63 : {v:int|false} -> {w:int|w=v ∧ w=1}
f63 = \x:int. x

f64 : {v:int|false} -> {w:int|false}
f64 = \x:int. x

f65 : {v:int|false} -> {w:int|?}
f65 = \x:int. x

f66 : {v:int|false} -> {w:int|w≠v}
f66 = \x:int. x

f74 : {v:int|?} -> {w:int|false}
f74 = \x:int. x
