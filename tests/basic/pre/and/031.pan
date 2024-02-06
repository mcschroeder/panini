and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f31 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|true}
f31 = \x:bool. \y:bool. and x y

-- NOTE
-- it would be wrong to infer "false" for x,
-- because even though "y=false" should imply "z=false" instead of "true",
-- the postcondition is allowed to be weaker, so there is no contradiction!
