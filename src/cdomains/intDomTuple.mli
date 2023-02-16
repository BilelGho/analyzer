open GoblintCil

(* module ManyInts : S *)
(* module IntDomList : S *)
module IntDomTuple : sig
  include IntDomain.Z
  val no_interval: t -> t
  val no_intervalSet: t -> t
  val ikind: t -> ikind
end

val of_const: Z.t * Cil.ikind * string option -> IntDomTuple.t
