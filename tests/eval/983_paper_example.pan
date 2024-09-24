import axioms

parser : {s:𝕊 | ?} → 𝟙
parser = λs:𝕊.
  let v0 = slice1 s 0 in
  let v1 = match v0 "a" in
  if v1 then
    let v2 = length s in
    let v3 = eq v2 1 in
    let _ = assert v3 in
    unit
  else
    let v4 = slice1 s 1 in
    let v5 = match v4 "b" in
    let _ = assert v5 in
    unit