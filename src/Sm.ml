module StringMap = Map.Make(String)

let check_dupes l =
  let rec add1 m (k, v) =
    if StringMap.mem k m then Resultx.error k
    else Resultx.ok (StringMap.add k v m) in
  Resultx.lfold add1 StringMap.empty l

module StringMapRx = Resultx.Maps (StringMap)
