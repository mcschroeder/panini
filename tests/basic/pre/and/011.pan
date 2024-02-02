and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f11 : {x:bool|?} -> {y:bool|?} -> {z:bool|z=false}
f11 = \x:bool. \y:bool. and x y

-- TODO: tbh, I'm not exactly sure what the best thing to infer here would be
