f0 = \x:int. unit

f1 : int -> unit
f1 = \x:int. unit

f2 : {v:int|?} -> {w:unit|?}
f2 = \x:int. unit

f3 : unit -> int      -- expected error
f3 = \x:int. unit

f4 : {v:unit|?} -> {w:int|?}      -- expected error
f4 = \x:int. unit

f5 = \x:int. 1

f6 : int -> int
f6 = \x:int. 1

f7 : {v:int|?} -> int
f7 = \x:int. 1

f8 : int -> {w:int|?}
f8 = \x:int. 1

f9 : {v:int|?} -> {w:int|?}
f9 = \x:int. 1

f10 : bool -> string      -- expected error
f10 = \x:int. 1

f11 = \x:int. \y:bool. unit

f12 : int -> bool -> unit
f12 = \x:int. \y:bool. unit

f13 = \x:(int -> int). \y:bool. unit

f14 : (int -> int) -> bool -> unit
f14 = \x:(int -> int). \y:bool. unit

f15 = \x:int. \y:(bool -> int). unit

f16 : int -> (bool -> int) -> unit
f16 = \x:int. \y:(bool -> int). unit
