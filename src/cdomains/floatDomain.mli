(** Abstract Domains for floats. These are domains that support the C
  * operations on double/float values. *)
open GoblintCil
open IntDomTuple

exception ArithmeticOnFloatBot of string

module type FloatArith = sig
  type t

  val neg : t -> t
  (** Negating a float value: [-x] *)
  val add : t -> t -> t
  (** Addition: [x + y] *)
  val sub : t -> t -> t
  (** Subtraction: [x - y] *)
  val mul : t -> t -> t
  (** Multiplication: [x * y] *)
  val div : t -> t -> t
  (** Division: [x / y] *)
  val fmax : t -> t -> t
  (** Maximum *)
  val fmin : t -> t -> t
  (** Minimum *)

  (** {unary functions} *)
  val ceil: t -> t
  (* ceil(x) *)
  val floor: t -> t
  (* floor(x) *)
  val fabs : t -> t
  (** fabs(x) *)
  val acos : t -> t
  (** acos(x) *)
  val asin : t -> t
  (** asin(x) *)
  val atan : t -> t
  (** atan(x) *)
  val cos : t -> t
  (** cos(x) *)
  val sin : t -> t
  (** sin(x) *)
  val tan : t -> t
  (** tan(x) *)


  (** {b Comparison operators} *)
  val lt : t -> t -> IntDomTuple.t
  (** Less than: [x < y] *)
  val gt : t -> t -> IntDomTuple.t
  (** Greater than: [x > y] *)
  val le : t -> t -> IntDomTuple.t
  (** Less than or equal: [x <= y] *)
  val ge : t -> t -> IntDomTuple.t
  (** Greater than or equal: [x >= y] *)
  val eq : t -> t -> IntDomTuple.t
  (** Equal to: [x == y] *)
  val ne : t -> t -> IntDomTuple.t
  (** Not equal to: [x != y] *)
  val unordered: t -> t -> IntDomTuple.t
  (** Unordered *)

  (** {unary functions returning int} *)
  val isfinite : t -> IntDomTuple.t
  (** __builtin_isfinite(x) *)
  val isinf : t -> IntDomTuple.t
  (** __builtin_isinf(x) *)
  val isnan : t -> IntDomTuple.t
  (** __builtin_isnan(x) *)
  val isnormal : t -> IntDomTuple.t
  (** __builtin_isnormal(x) *)
  val signbit : t -> IntDomTuple.t
  (** __builtin_signbit(x) *)
end

module type FloatDomainBase = sig
  include Lattice.S
  include FloatArith with type t := t

  val to_int : Cil.ikind -> t -> IntDomTuple.t

  val nan: unit -> t

  val of_const : float -> t
  val of_interval : float * float -> t
  val of_string : string -> t
  val of_int: IntDomTuple.t -> t

  val ending : float -> t
  val starting : float -> t
  val ending_before : float -> t
  val starting_after : float -> t

  val minimal: t -> float option
  val maximal: t -> float option

  val is_exact : t -> bool
end

(* Only exposed for testing *)
module F64Interval : FloatDomainBase
module F32Interval : FloatDomainBase

module type FloatDomain = sig
  include Lattice.S
  include FloatArith with type t := t

  val to_int : Cil.ikind -> t -> IntDomTuple.t
  val cast_to : Cil.fkind -> t -> t

  val of_const : Cil.fkind -> float -> t
  val of_interval : Cil.fkind -> float*float -> t
  val of_string : Cil.fkind -> string -> t
  val of_int: Cil.fkind -> IntDomTuple.t -> t

  val top_of: Cil.fkind -> t
  val bot_of: Cil.fkind -> t

  val nan_of: Cil.fkind -> t
  val inf_of: Cil.fkind -> t
  val minus_inf_of: Cil.fkind -> t

  val ending : Cil.fkind -> float -> t
  val starting : Cil.fkind -> float -> t
  val ending_before : Cil.fkind -> float -> t
  val starting_after : Cil.fkind -> float -> t

  val minimal: t -> float option
  val maximal: t -> float option

  val is_exact : t -> bool
  val get_fkind : t -> Cil.fkind
  val invariant: Cil.exp -> t -> Invariant.t
end

module FloatDomTupleImpl : FloatDomain
