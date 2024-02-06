and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f71 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|true}
f71 = \x:bool. \y:bool. and x y

-- NOTE
-- it would be wrong to infer "false" for y
-- because even though "x=false" should imply "z=false" instead of "true",
-- the postcondition is allowed to be weaker, so there is no contradiction!
