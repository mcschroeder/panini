assume eq_string : s:string -> t:string -> {b:bool | b <=> s = t}
assume eq_nat : {n:int | n >= 0} -> {m:int | n >= 0} -> {b:bool | b <=> m = n}
assume drop : {n:int | n >= 0} -> s:string -> {t:string | s = "Sigma^n ++ t"}
assume length : s:string -> { n:int | n >= 0 /\ n = length(s) }

assume take :
  {n:int | n >= 0} ->
  { s:string | length(s) >= n } -> 
  { t:string | t = "Sigma^n" /\ s = "t ++ Sigma^*" }

assume empty : s:string -> {b : bool | b <=> length(s) = 0}
define empty = \x. 
  let n = length x in
  eq_nat 0 n

assume trim : s:string -> {t:string | ?}
define trim = \x.
  let b = empty x in
  if b then "" else
    let y = take 1 x in
    let p = eq_string " " y in
    if p then 
      let z = drop 1 x in
      trim z
    else x
      
