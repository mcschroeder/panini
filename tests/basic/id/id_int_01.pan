id : (i:ℤ) → { j:ℤ | j = i }

x00 : int
x00 = id 0

x01 : {n:int|n=0}
x01 = id 0

x02 : {n:int|n>=0}
x02 = id 0

x10 : int
x10 = id 1

x11 : {n:int|n=1}
x11 = id 1

x12 : {n:int|n>0}
x12 = id 1

x20 : int
x20 = id -1

x21 : {n:int|n=-1}
x21 = id -1

x22 : {n:int|n<0}
x22 = id -1
