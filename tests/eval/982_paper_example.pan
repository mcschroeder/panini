import axioms

parser : {s:𝕊 | ?} → 𝟙
parser = λs:𝕊.
  let L0 = unit in
  rec L1 : {s1:string|?} -> unit = \s1:string.
    let v4 = slice1 s1 1 in
    let v5 = match v4 "b" in
    let _ = assert v5 in
    L0
  in
    rec L2 : {s2:string|?} -> unit = \s2:string.
      let v2 = length s2 in
      let v3 = eq v2 1 in
      let _ = assert v3 in
      L0
    in
      let v0 = slice1 s 0 in
      let v1 = match v0 "a" in
      if v1 then L2 s else L1 s
