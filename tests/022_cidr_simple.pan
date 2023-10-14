is_valid_cidr = \(string_network:string).
  let n1 = str_count string_network "/" in
  let p1 = int_eq n1 1 in
  let _ = assert p1 in
  let slash_index = str_index string_network "/" in
  let i1 = int_add slash_index 1 in
  let i2 = str_length slash_index in
  let i3 = int_sub i2 1 in
  let s1 = str_sub string_network i1 i3 in
  let mask = str_to_int s1 in
  let p2 = int_ge mask 1 in
  let p3 = int_le mask 32 in
  let p4 = and p2 p3 in
  let _ = assert p4 in
  let i4 = int_sub slash_index 1 in
  let s2 = str_sub string_network 0 i4 in
  let _ = inet_aton s2 in
  unit
