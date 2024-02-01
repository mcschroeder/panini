
f00 = \b:bool. if b then 1 else 2

f10 : {b:bool|?} -> {v:int|?}
f10 = \b:bool. if b then 1 else 2

f11 : {b:bool|true} -> {v:int|?}
f11 = \b:bool. if b then 1 else 2

f12 : {b:bool|?} -> {v:int|v=1 \/ v = 2}
f12 = \b:bool. if b then 1 else 2

f13 : {b:bool|true} -> {v:int|v=1 \/ v = 2}
f13 = \b:bool. if b then 1 else 2

f32 : {b:bool|false} -> {v:int|?}
f32 = \b:bool. if b then 1 else 2
