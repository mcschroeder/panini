import axioms

parser : {s:string|?} -> unit
parser = λs:𝕊.
  let x = slice1 s 0 in
  let p1 = match x "a" in
  if p1 then
    let n = length s in
    let p2 = eq n 1 in
    assert p2
  else
    let y = slice1 s 1 in
    let p3 = match y "b" in
    assert p3
