id : (i:ℤ) → { j:ℤ | j = i }

f40 : {n:int|n≠0}
f40 = id 0

f41 : {n:int|n=1}
f41 = id 0

f42 : {n:int|n>0}
f42 = id 0

f43 : {n:int|n=0}
f43 = id 1

f44 : {n:int|n>1}
f44 = id 1

f45 : {n:int|n=-2}
f45 = id -1

f46 : {n:int|n>-1}
f46 = id -1

