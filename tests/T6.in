add : a:int -> b:int -> {c:int | c = a+b}

apply = \f:(int -> int).\x:int. f x

incr = \n:int. add n 1

two = apply incr 1
