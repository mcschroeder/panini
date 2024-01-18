not : (x:bool) -> {y:bool|y = ~x}

f0 = not true

f1 = not false

f2 : bool
f2 = not true

f3 : bool
f3 = not false

f4 : {b:bool|?}
f4 = not true

f5 : {b:bool|?}
f5 = not false


-- alternative definition of not:

not_iff : (x:bool) -> {y:bool|y = true <=> x = false}

g0 = not_iff true

g1 = not_iff false

g2 : bool
g2 = not_iff true

g3 : bool
g3 = not_iff false

g4 : {b:bool|?}
g4 = not_iff true

g5 : {b:bool|?}
g5 = not_iff false