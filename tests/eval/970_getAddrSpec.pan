import axioms

getAddrSpec : {email:string|?} -> string
getAddrSpec = \email:string.
  let b1 = index email '<' in
  let p1 = ge b1 0 in
  let _ = assert p1 in
  let b2 = add b1 1 in  
  let v1 = index email '>' in
  let v2 = ge v1 b2 in
  let _ = assert v2 in  
  let v3 = sub v1 1 in  
  slice email b2 v3
