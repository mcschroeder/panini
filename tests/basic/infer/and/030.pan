and : (a:bool) -> (b:bool) -> {c:bool| c = true <=> (a = true /\ b = true)}

f30 = 
  let p1 = true in
  let p2 = true in
  and p1 p2

f31 = 
  let p1 = true in
  let p2 = false in
  and p1 p2

f32 = 
  let p1 = false in
  let p2 = true in
  and p1 p2

f33 = 
  let p1 = false in
  let p2 = false in
  and p1 p2
