import axioms

parser : {s:string|?} -> unit
parser = λs:𝕊.
  let x = charAt s 0 in
  let p1 = eqChar x 'a' in
  if p1 then
    let n = length s in
    let p2 = eq n 1 in
    assert p2
  else
    let y = charAt s 1 in
    let p3 = eqChar y 'b' in
    assert p3
