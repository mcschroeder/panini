import axioms

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
