
f100 : {b:bool|false} -> {v:int|true}
f100 = \b:bool. if b then 1 else 2

f101 : {b:bool|false} -> {v:int|false}
f101 = \b:bool. if b then 1 else 2

f102 : {b:bool|false} -> {v:int|v=3}
f102 = \b:bool. if b then 1 else 2
