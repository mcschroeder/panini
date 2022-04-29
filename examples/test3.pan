assume le : (a:int) -> (b:int) -> {c:bool| c <=> a < b}

assume test : int -> bool
define test = \x. le x 1