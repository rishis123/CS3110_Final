module Make (H : Hashtbl.HashedType) = struct
  module HT = Hashtbl.Make (H)
  
  let memoize ?(n = 10) f =
    let lookup = HT.create n in
    let rec memoized x =
      if HT.mem lookup x then HT.find lookup x
      else
        let result = f memoized x in
        HT.add lookup x result;
        result
    in
    memoized
end
