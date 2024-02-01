and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}


f20 : {z:bool|z=false}
f20 = 
  let p1 = true in
  let p2 = true in
  and p1 p2

f21 : {z:bool|z=true}
f21 = 
  let p1 = true in
  let p2 = false in
  and p1 p2

f22 : {z:bool|z=true}
f22 = 
  let p1 = false in
  let p2 = true in
  and p1 p2

f23 : {z:bool|z=true}
f23 = 
  let p1 = false in
  let p2 = false in
  and p1 p2
