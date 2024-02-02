and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f40 : {z:bool|?}
f40 =  
  let p1 = true in
  let p2 = true in
  and p1 p2

f41 : {z:bool|?}
f41 =  
  let p1 = true in
  let p2 = false in
  and p1 p2

f42 : {z:bool|?}
f42 =  
  let p1 = false in
  let p2 = true in
  and p1 p2

f43 : {z:bool|?}
f43 =  
  let p1 = false in
  let p2 = false in
  and p1 p2

