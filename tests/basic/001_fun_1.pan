f00 = \x:int. 1

----------------------------------------

f10 : int -> int
f10 = \x:int. 1

----------------------------------------

f20 : {v:int|true} -> int
f20 = \x:int. 1

f21 : {v:int|v=1} -> int
f21 = \x:int. 1

f22 : {v:int|v=2} -> int
f22 = \x:int. 1

f23 : {v:int|false} -> int
f23 = \x:int. 1

f24 : {v:int|?} -> int
f24 = \x:int. 1

----------------------------------------

f30 : int -> {w:int|true}
f30 = \x:int. 1

f31 : int -> {w:int|w=1}
f31 = \x:int. 1

f32 : int -> {w:int|w=2}        -- error expected
f32 = \x:int. 1

f33 : int -> {w:int|false}      -- error expected
f33 = \x:int. 1

f34 : int -> {w:int|?}
f34 = \x:int. 1

----------------------------------------

f40 : {v:int|true} -> {w:int|true}
f40 = \x:int. 1

f41 : {v:int|true} -> {w:int|w=1}
f41 = \x:int. 1

f42 : {v:int|true} -> {w:int|w=2}     -- error expected
f42 = \x:int. 1

f43 : {v:int|true} -> {w:int|false}   -- error expected
f43 = \x:int. 1

f44 : {v:int|true} -> {w:int|?}
f44 = \x:int. 1

----------------------------------------

f50 : {v:int|v=1} -> {w:int|true}
f50 = \x:int. 1

f51 : {v:int|v=1} -> {w:int|w=1}
f51 = \x:int. 1

f52 : {v:int|v=1} -> {w:int|w=2}      -- error expected
f52 = \x:int. 1

f53 : {v:int|v=1} -> {w:int|false}    -- error expected
f53 = \x:int. 1

f54 : {v:int|v=1} -> {w:int|?}
f54 = \x:int. 1

----------------------------------------

f60 : {v:int|v=2} -> {w:int|true}
f60 = \x:int. 1

f61 : {v:int|v=2} -> {w:int|w=1}
f61 = \x:int. 1

f62 : {v:int|v=2} -> {w:int|w=2}      -- error expected
f62 = \x:int. 1

f63 : {v:int|v=2} -> {w:int|false}    -- error expected
f63 = \x:int. 1

f64 : {v:int|v=2} -> {w:int|?}
f64 = \x:int. 1

----------------------------------------

f70 : {v:int|false} -> {w:int|true}
f70 = \x:int. 1

f71 : {v:int|false} -> {w:int|w=1}
f71 = \x:int. 1

f72 : {v:int|false} -> {w:int|w=2}
f72 = \x:int. 1

f73 : {v:int|false} -> {w:int|false}
f73 = \x:int. 1

f74 : {v:int|false} -> {w:int|?}
f74 = \x:int. 1

----------------------------------------

f80 : {v:int|?} -> {w:int|true}
f80 = \x:int. 1

f81 : {v:int|?} -> {w:int|w=1}
f81 = \x:int. 1

f82 : {v:int|?} -> {w:int|w=2}
f82 = \x:int. 1

f83 : {v:int|?} -> {w:int|false}
f83 = \x:int. 1

----------------------------------------

f90 : {v:int|?} -> {w:int|?}
f90 = \x:int. 1
