and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}
eq : (a:ℤ) → (b:ℤ) → { c:𝔹 | c = true ⟺ a = b }

f0 : {z:bool|?}
f0 =
  let p1 = eq 1 1 in
  let p2 = eq 1 1 in
  and p1 p2

f1 : {z:bool|?}
f1 =
  let p1 = eq 1 1 in
  let p2 = eq 2 1 in
  and p1 p2

f2 : {x:int|?} -> {z:bool|?}
f2 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  and p1 p2

f3 : {x:int|x=1} -> {z:bool|?}
f3 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  and p1 p2

f4 : {x:int|x=2} -> {z:bool|?}
f4 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  and p1 p2

f5 : {x:int|?} -> {z:bool|z=true}
f5 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  and p1 p2

f6 : {x:int|?} -> {z:bool|z=false}
f6 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  and p1 p2

f7 : {x:int|x=1} -> {z:bool|z=true}
f7 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  and p1 p2

f8 : {x:int|x=2} -> {z:bool|z=true}      -- expected error
f8 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  and p1 p2

f9 : {x:int|x=1} -> {z:bool|z=false}    -- expected error
f9 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  and p1 p2

f10 : {x:int|x=2} -> {z:bool|z=false}
f10 = \x:int.
  let p1 = eq x 1 in
  let p2 = eq x 1 in
  and p1 p2

f11 : {x:int|?} -> {y:int|?} -> {z:bool|?}
f11 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f12 : {x:int|x=1} -> {y:int|?} -> {z:bool|?}
f12 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f13 : {x:int|x=2} -> {y:int|?} -> {z:bool|?}
f13 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f14 : {x:int|?} -> {y:int|y=1} -> {z:bool|?}
f14 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f15 : {x:int|?} -> {y:int|y=2} -> {z:bool|?}
f15 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f16 : {x:int|?} -> {y:int|?} -> {z:bool|z=true}
f16 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f17 : {x:int|?} -> {y:int|?} -> {z:bool|z=false}
f17 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f18 : {x:int|x=1} -> {y:int|?} -> {z:bool|z=true}
f18 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f19 : {x:int|x=2} -> {y:int|?} -> {z:bool|z=true}
f19 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f20 : {x:int|x=1} -> {y:int|?} -> {z:bool|z=false}
f20 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f21 : {x:int|x=2} -> {y:int|?} -> {z:bool|z=false}
f21 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f22 : {x:int|?} -> {y:int|y=1} -> {z:bool|z=true}
f22 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f23 : {x:int|?} -> {y:int|y=2} -> {z:bool|z=true}
f23 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f24 : {x:int|?} -> {y:int|y=1} -> {z:bool|z=false}
f24 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2

f25 : {x:int|?} -> {y:int|y=2} -> {z:bool|z=false}
f25 = \x:int. \y:int.
  let p1 = eq x 1 in
  let p2 = eq y 1 in
  and p1 p2
