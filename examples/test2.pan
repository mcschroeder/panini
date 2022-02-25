type nat = { n : int | n >= 0 }

assume eq_string : s:string -> t:string -> {b:bool | b <=> s = t}
assume eq_nat : n:nat -> m:nat -> {b:bool | b <=> m = n}
assume drop : n:nat -> s:string -> {t:string | s = Sigma^n ++ t}
assume length : s:string -> { n:nat | n = length(s) }

assume take :
  n:nat -> 
  { s:string | length(s) >= n } -> 
  { t:string | t = Sigma^n && s = t ++ Sigma^* }

def empty : s:string -> {b : bool | b <=> length(s) = 0}
= \x. 
  let n = length x
  eq_nat 0 n


def trim : s:string -> {t:string | ?}
= \x.
  let b = empty x
  if b then epsilon else
    let y = take 1 x
    let p = eq_string " " y
    if !p then x else
      let z = drop 1 x
      trim z
