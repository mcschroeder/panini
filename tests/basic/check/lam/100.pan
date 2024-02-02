f100 : {v:int|false} -> int
f100 = \x:int. x

f101 : {v:int|false} -> {w:int|true}
f101 = \x:int. x

f102 : {v:int|false} -> {w:int|w=v}
f102 = \x:int. x

f103 : {v:int|false} -> {w:int|w=1}
f103 = \x:int. x

f104 : {v:int|false} -> {w:int|w=v ∧ w=1}
f104 = \x:int. x

f105 : {v:int|false} -> {w:int|false}
f105 = \x:int. x

f106 : {v:int|false} -> {w:int|?}
f106 = \x:int. x

f107 : {v:int|false} -> {w:int|w≠v}
f107 = \x:int. x

