str_in : (s:string) -> (t:string) -> {b:bool| b = str_in(s,t) }
not : (b:bool) -> { v:bool | v = ~b }
str_index : (s:string) -> (t:string) -> {i:int| i = str_index(s,t) }
str_sub : (s:string) -> (i:int) -> (j:int) -> {t:string| t = s[i..j] }
re_match : (r:string) -> (s:string) -> {b:bool| b = true <=> s \in r }

get_alias_from_email = \(email:string).
  let p1 = str_in "@" email in
  let p2 = not p1 in
  if p2 then
    ""
  else
    let i = str_index email "@" in
    let potential_alias = str_sub email 0 i in
    let p3 = re_match "^[a-zA-Z0-9\\-]+$" potential_alias in
    if p3 then
      potential_alias
    else
      ""
