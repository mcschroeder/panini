type nat = { n : int | n >= 0 } in
let eq_string = bot : s:string -> t:string -> {b:bool | b <=> s = t} in
let eq_nat = bot : (n : nat) -> (m : nat) -> { b : bool | b <=> m = n } in
let drop = bot : (n : nat) -> (s : string) -> { t : string | s = \Sigma^n <> t } in
let take = bot : (n : nat) -> { s : string | |s| >= n } -> { t : string | t = \Sigma^n && s = t\Sigma^* } in
let length = bot : (s : string) -> { n: nat | n = |s| } in
rec empty : s:string -> {b : bool | b <=> length(s) = 0}
= \x. 
  let n = length x in 
  eq_nat 0 n
in
rec trim : {s : string | ?} -> {t : string | ?}
= \x.
  let b = empty x in
  if b then 
    epsilon
  else
    let y = take 1 x in
    let p = eq_string " " y in
    if p then
      let z = drop 1 x in
      trim z
    else 
      x
in
unit
