type nat = { n : int | n >= 0 }

assume eq_string : (s : string) -> (t : string) -> { b : bool | b <=> s = t }
assume eq_nat : (n : nat) -> (m : nat) -> { b : bool | b <=> m = n }
assume drop : (n : nat) -> (s : string) -> { t : string | s = \Sigma^n <> t }
assume take : (n : nat) -> { s : string | |s| >= n } -> { t : string | t = \Sigma^n, s = t\Sigma^* }
assume length : (s : string) -> { n: nat | n = |s| }

rec empty : (s : string) -> { b : bool | b <=> |s| = 0 }
= \x. 
  let n = length x
  eq_nat 0 n


rec trim : { s : string | ? } -> { t : string | ? }
= \x.
  let b = empty x
  if b then 
    epsilon
  else
    let y = take 1 x
    let x = eq_string " " y
    if x then
      let z = drop 1 x
      trim z
    else 
      x

