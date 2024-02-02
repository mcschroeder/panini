id : (i:ℤ) → { j:ℤ | j = i }

f30 : {n:int|n>=0}
f30 = id 0

f31 : {n:int|n>0}
f31 = id 1

f32 : {n:int|n<0}
f32 = id -1
