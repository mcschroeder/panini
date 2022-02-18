let parse = \s. 
  let n = length s : int {v | length(s) = v and v >= 0} in
  let m = 5 in
  let p = eq n m : bool {v | (v = true and n = m) or (v = false and n /= m)} in
  if p then true else false
in parse : (s : string {?}) -> bool
