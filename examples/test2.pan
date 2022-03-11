import prelude
import lang.c
import lang.python

type nat = { n : int | n >= 0 }

eq_string : s:string -> t:string -> {b:bool | b <=> s = t}
eq_nat : n:nat -> m:nat -> {b:bool | b <=> m = n}
drop : n:nat -> s:string -> {t:string | s = Sigma^n ++ t}
length : s:string -> { n:nat | n = length(s) }

take :
  n:nat -> 
  { s:string | length(s) >= n } -> 
  { t:string | t = Sigma^n && s = t ++ Sigma^* }

empty : s:string -> {b : bool | b <=> length(s) = 0}
empty = \x. 
  let n = length x
  eq_nat 0 n

trim : s:string -> {t:string | ?}
trim = \x.
  let b = empty x
  if b then epsilon else
    let y = take 1 x
    let p = eq_string " " y
    if !p then x else
      let z = drop 1 x
      trim z
