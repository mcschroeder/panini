assume inc : x:int -> {v:int|v = x + 1}
assume dec : x:int -> {v:int|v = x - 1}

assume ex1 : {a:int|a >= 0} -> {b:int|b >= 0}
define ex1 = \x.
  let y =
    let t = x
    in
      dec t
  in
    inc y
