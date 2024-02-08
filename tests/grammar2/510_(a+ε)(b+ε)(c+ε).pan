import axioms

f510 : {s:string|?} -> unit
f510 = \s:string.
  let p1 = match s "abc" in
  let p2 = match s "ab" in
  let p3 = match s "a" in
  let p4 = match s "ac" in
  let p5 = match s "bc" in
  let p6 = match s "b" in
  let p7 = match s "c" in
  let p8 = match s "" in
  let q1 = or p1 p2 in
  let q2 = or p3 p4 in
  let q3 = or p5 p6 in
  let q4 = or p7 p8 in
  let r1 = or q1 q2 in
  let r2 = or q3 q4 in
  let t1 = or r1 r2 in
  assert t1

f511 : {s:string|?} -> unit
f511 = \s:string.
  let p1 = match s "abc" in
  let p2 = match s "ab" in
  let p3 = match s "a" in
  let p4 = match s "ac" in
  let p5 = match s "bc" in
  let p6 = match s "b" in
  let p7 = match s "c" in
  let p8 = match s "" in
  let q1 = or p1 p2 in
  let q2 = or q1 p3 in
  let q3 = or q2 p4 in
  let q4 = or q3 p5 in
  let q5 = or q4 p6 in
  let q6 = or q5 p7 in
  let q7 = or q6 p8 in
  assert q7

f512 : {s:string|?} -> unit
f512 = \s:string.
  let p1 = match s "abc" in
  if p1 then unit else
    let p2 = match s "ab" in
    if p2 then unit else
      let p3 = match s "a" in
      if p3 then unit else
        let p4 = match s "ac" in
        if p4 then unit else
          let p5 = match s "bc" in
          if p5 then unit else
            let p6 = match s "b" in
            if p6 then unit else
              let p7 = match s "c" in
              if p7 then unit else
                let p8 = match s "" in
                if p8 then unit else
                  assert false
