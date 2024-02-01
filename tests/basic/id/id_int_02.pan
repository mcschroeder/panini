id : (i:ℤ) → { j:ℤ | j = i }

x00 : {v:int|false}
x00 = id 0

x01 : {n:int|n=1}
x01 = id 0

x02 : {n:int|n>0}
x02 = id 0

x11 : {n:int|n=0}
x11 = id 1

x12 : {n:int|n>1}
x12 = id 1

x21 : {n:int|n=-2}
x21 = id -1

x22 : {n:int|n>-1}
x22 = id -1
