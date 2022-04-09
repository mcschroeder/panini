assume inc : a:int -> {b:int|b = a+1}
assume dec : a:int -> {b:int|b = a-1}

assume ex1 : {a:int|a >= 0} -> {b:int|?}
define ex1 = \x.
  let y =
    (let t = x in
    dec t) : {v:int|?}
  in 
  inc y
