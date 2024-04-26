module Make : functor (H : Hashtbl.HashedType) -> sig
  val memoize : ?n:int -> ((H.t -> 'a) -> H.t -> 'a) -> H.t -> 'a
  (** [memoize f r n] memoizes the function [f r] on inputs [x] with [n]
      expected distinct calls to [r], where [r] is a recursive call *)
end
