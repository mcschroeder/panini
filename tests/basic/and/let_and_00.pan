and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f00 = 
  let p1 = true in
  let p2 = true in
  and p1 p2

f01 = 
  let p1 = true in
  let p2 = false in
  and p1 p2

f02 = 
  let p1 = false in
  let p2 = true in
  and p1 p2

f03 = 
  let p1 = false in
  let p2 = false in
  and p1 p2


f10 : {z:bool|?}
f10 =  
  let p1 = true in
  let p2 = true in
  and p1 p2

f11 : {z:bool|?}
f11 =  
  let p1 = true in
  let p2 = false in
  and p1 p2

f12 : {z:bool|?}
f12 =  
  let p1 = false in
  let p2 = true in
  and p1 p2

f13 : {z:bool|?}
f13 =  
  let p1 = false in
  let p2 = false in
  and p1 p2


f20 : {z:bool|z=true}
f20 = 
  let p1 = true in
  let p2 = true in
  and p1 p2

f21 : {z:bool|z=false}
f21 = 
  let p1 = true in
  let p2 = false in
  and p1 p2

f22 : {z:bool|z=false}
f22 = 
  let p1 = false in
  let p2 = true in
  and p1 p2

f23 : {z:bool|z=false}
f23 = 
  let p1 = false in
  let p2 = false in
  and p1 p2
