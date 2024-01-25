f00 = \x:int. x

----------------------------------------

f10 : int -> int
f10 = \x:int. x

----------------------------------------

f20 : {v:int|true} -> int
f20 = \x:int. x

f21 : {v:int|v=1} -> int
f21 = \x:int. x

f22 : {v:int|false} -> int
f22 = \x:int. x

f23 : {v:int|?} -> int
f23 = \x:int. x

----------------------------------------

f30 : int -> {w:int|true}
f30 = \x:int. x

--f31 : int -> {w:int|w=v}
--f31 = \x:int. x

f32 : int -> {w:int|w=2}      -- error expected
f32 = \x:int. x

f33 : int -> {w:int|false}    -- error expected
f33 = \x:int. x

f34 : int -> {w:int|?}
f34 = \x:int. x

----------------------------------------

f40 : {v:int|true} -> {w:int|true}
f40 = \x:int. x

f41 : {v:int|true} -> {w:int|w=v}
f41 = \x:int. x

f42 : {v:int|true} -> {w:int|w=1}           -- error expected
f42 = \x:int. x

f43 : {v:int|true} -> {w:int|w=v ∧ w=1}     -- error expected
f43 = \x:int. x

f44 : {v:int|true} -> {w:int|false}         -- error expected
f44 = \x:int. x

f45 : {v:int|true} -> {w:int|?}
f45 = \x:int. x

----------------------------------------

f50 : {v:int|v=1} -> {w:int|true}
f50 = \x:int. x

f51 : {v:int|v=1} -> {w:int|w=v}
f51 = \x:int. x

f52 : {v:int|v=1} -> {w:int|w=1}
f52 = \x:int. x

f53 : {v:int|v=1} -> {w:int|w=v ∧ w=1}
f53 = \x:int. x

f54 : {v:int|v=1} -> {w:int|false}      -- error expected
f54 = \x:int. x

f55 : {v:int|v=1} -> {w:int|?}
f55 = \x:int. x

----------------------------------------

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

----------------------------------------

f70 : {v:int|?} -> {w:int|true}
f70 = \x:int. x

f71 : {v:int|?} -> {w:int|w=v}
f71 = \x:int. x

f72 : {v:int|?} -> {w:int|w=1}
f72 = \x:int. x

f73 : {v:int|?} -> {w:int|w=v ∧ w=1}
f73 = \x:int. x

f74 : {v:int|?} -> {w:int|false}
f74 = \x:int. x

----------------------------------------

f80 : {v:int|?} -> {w:int|?}
f80 = \x:int. x

