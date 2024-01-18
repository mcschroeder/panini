and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f0 : {z:bool|?}
f0 = and true true

f1 : {z:bool|?}
f1 = and true false

f2 : {z:bool|?}
f2 = and false true

f3 : {z:bool|?}
f3 = and false false

f4 : {z:bool|?}
f4 =
  let p1 = true in
  let p2 = true in
  and p1 p2

f5 : {z:bool|?}
f5 =
  let p1 = true in
  let p2 = false in
  and p1 p2

f6 : {z:bool|?}
f6 =
  let p1 = false in
  let p2 = true in
  and p1 p2

f7 : {z:bool|?}
f7 =
  let p1 = false in
  let p2 = false in
  and p1 p2

f8 : {x:bool|?} -> {y:bool|?} -> {z:bool|?}
f8 = \p1:bool. \p2:bool. and p1 p2

f9 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|?}
f9 = \p1:bool. \p2:bool. and p1 p2

f10 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|?}
f10 = \p1:bool. \p2:bool. and p1 p2

f11 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|?}
f11 = \p1:bool. \p2:bool. and p1 p2

f12 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|?}
f12 = \p1:bool. \p2:bool. and p1 p2

f13 : {x:bool|?} -> {y:bool|?} -> {z:bool|z=true}
f13 = \p1:bool. \p2:bool. and p1 p2

f14 : {x:bool|?} -> {y:bool|?} -> {z:bool|z=false}
f14 = \p1:bool. \p2:bool. and p1 p2

f15 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|z=true}
f15 = \p1:bool. \p2:bool. and p1 p2

f16 : {x:bool|x=true} -> {y:bool|?} -> {z:bool|z=false}
f16 = \p1:bool. \p2:bool. and p1 p2

f17 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|z=true}
f17 = \p1:bool. \p2:bool. and p1 p2

f18 : {x:bool|x=false} -> {y:bool|?} -> {z:bool|z=false}
f18 = \p1:bool. \p2:bool. and p1 p2

f19 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|z=true}
f19 = \p1:bool. \p2:bool. and p1 p2

f20 : {x:bool|?} -> {y:bool|y=true} -> {z:bool|z=false}
f20 = \p1:bool. \p2:bool. and p1 p2

f21 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|z=true}
f21 = \p1:bool. \p2:bool. and p1 p2

f22 : {x:bool|?} -> {y:bool|y=false} -> {z:bool|z=false}
f22 = \p1:bool. \p2:bool. and p1 p2

f23 : {x:bool|?} -> {y:bool|?} -> {z:bool|?}
f23 = and
