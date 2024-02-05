and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f05 : {z:bool|?}
f05 =  
  let p1 = true in
  let p2 = true in
  and p1 p2

f06 : {z:bool|?}
f06 =  
  let p1 = true in
  let p2 = false in
  and p1 p2

f07 : {z:bool|?}
f07 =  
  let p1 = false in
  let p2 = true in
  and p1 p2

f08 : {z:bool|?}
f08 =  
  let p1 = false in
  let p2 = false in
  and p1 p2

