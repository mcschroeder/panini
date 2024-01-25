a0 = unit
b0 = true
c0 = 1
d0 = "hello"

----------------------------------------

a1 : unit
a1 = unit

b1 : bool
b1 = true

c1 : int
c1 = 1

d1 : string
d1 = "hello"

----------------------------------------

a2 : {v:unit|true}
a2 = unit

b2 : {v:bool|true}
b2 = true

c2 : {v:int|true}
c2 = 1

d2 : {v:string|true}
d2 = "hello"

----------------------------------------

a3 : {v:unit|v=unit}
a3 = unit

b3 : {v:bool|v=true}
b3 = true

c3 : {v:int|v=1}
c3 = 1

d3 : {v:string|v="hello"}
d3 = "hello"

----------------------------------------

a4 : {v:unit|false}
a4 = unit

b4 : {v:bool|false}
b4 = true

c4 : {v:int|false}
c4 = 1

d4 : {v:string|false}
d4 = "hello"

----------------------------------------

a5 : {v:unit|?}
a5 = unit

b5 : {v:bool|?}
b5 = true

c5 : {v:int|?}
c5 = 1

d5 : {v:string|?}
d5 = "hello"

----------------------------------------
-- all below are expected to fail

a6 : int
a6 = unit

b6 : unit
b6 = true

c6 : unit
c6 = 1

d6 : unit
d6 = "hello"

---------------------------

a7 : {v:int|?}
a7 = unit

b7 : {v:unit|?}
b7 = true

c7 : {v:unit|?}
c7 = 1

d7 : {v:unit|?}
d7 = "hello"

---------------------------

-- NOTE
-- this one is actually not expected to fail, 
-- because "v ≠ unit" reduces to "false"
a8 : {v:unit|v≠unit}
a8 = unit

b8 : {v:bool|v≠true}
b8 = true

c8 : {v:int|v≠1}
c8 = 1

d8 : {v:string|v≠"hello"}
d8 = "hello"

---------------------------

--a9 : {v:unit|v≠unit}
--a9 = unit

b9 : {v:bool|v=false}
b9 = true

c9 : {v:int|v=2}
c9 = 1

d9 : {v:string|v="goodbye"}
d9 = "hello"
