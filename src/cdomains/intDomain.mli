(** Abstract Domains for integers. These are domains that support the C
  * operations on integer values. *)
open GoblintCil

val should_wrap: Cil.ikind -> bool
val should_ignore_overflow: Cil.ikind -> bool

val set_overflow_flag: cast:bool -> underflow:bool -> overflow:bool -> ikind -> unit
val widening_thresholds: Z.t list ResettableLazy.t
val widening_thresholds_desc: Z.t list ResettableLazy.t

val reset_lazy: unit -> unit

module type Arith =
sig
  type t
  (** {b Arithmetic operators} *)

  val neg: t -> t
  (** Negating an integer value: [-x] *)

  val add: t -> t -> t
  (** Addition: [x + y] *)

  val sub: t -> t -> t
  (** Subtraction: [x - y] *)

  val mul: t -> t -> t
  (** Multiplication: [x * y] *)

  val div: t -> t -> t
  (** Division: [x / y] *)

  val rem: t -> t -> t
  (** Integer remainder: [x % y] *)


  (** {b Comparison operators} *)

  val lt: t -> t -> t
  (** Less than: [x < y] *)

  val gt: t -> t -> t
  (** Greater than: [x > y] *)

  val le: t -> t -> t
  (** Less than or equal: [x <= y] *)

  val ge: t -> t -> t
  (** Greater than or equal: [x >= y] *)

  val eq: t -> t -> t
  (** Equal to: [x == y] *)

  val ne: t -> t -> t
  (** Not equal to: [x != y] *)


  (** {b Bit operators} *)

  val bitnot: t -> t
  (** Bitwise not (one's complement): [~x] *)

  val bitand: t -> t -> t
  (** Bitwise and: [x & y] *)

  val bitor : t -> t -> t
  (** Bitwise or: [x | y] *)

  val bitxor: t -> t -> t
  (** Bitwise exclusive or: [x ^ y] *)

  val shift_left : t -> t -> t
  (** Shifting bits left: [x << y] *)

  val shift_right: t -> t -> t
  (** Shifting bits right: [x >> y] *)


  (** {b Logical operators} *)

  val lognot: t -> t
  (** Logical not: [!x] *)

  val logand: t -> t -> t
  (** Logical and: [x && y] *)

  val logor : t -> t -> t
  (** Logical or: [x || y] *)

end

module type ArithIkind =
sig
  type t
  (** {b Arithmetic operators} *)

  val neg: Cil.ikind -> t -> t
  (** Negating an integer value: [-x] *)

  val add: Cil.ikind -> t -> t -> t
  (** Addition: [x + y] *)

  val sub: Cil.ikind -> t -> t -> t
  (** Subtraction: [x - y] *)

  val mul: Cil.ikind -> t -> t -> t
  (** Multiplication: [x * y] *)

  val div: Cil.ikind -> t -> t -> t
  (** Division: [x / y] *)

  val rem: Cil.ikind -> t -> t -> t
  (** Integer remainder: [x % y] *)


  (** {b Comparison operators} *)

  val lt: Cil.ikind -> t -> t -> t
  (** Less than: [x < y] *)

  val gt: Cil.ikind -> t -> t -> t
  (** Greater than: [x > y] *)

  val le: Cil.ikind -> t -> t -> t
  (** Less than or equal: [x <= y] *)

  val ge: Cil.ikind -> t -> t -> t
  (** Greater than or equal: [x >= y] *)

  val eq: Cil.ikind -> t -> t -> t
  (** Equal to: [x == y] *)

  val ne: Cil.ikind -> t -> t -> t
  (** Not equal to: [x != y] *)


  (** {b Bit operators} *)

  val bitnot: Cil.ikind -> t -> t
  (** Bitwise not (one's complement): [~x] *)

  val bitand: Cil.ikind -> t -> t -> t
  (** Bitwise and: [x & y] *)

  val bitor : Cil.ikind -> t -> t -> t
  (** Bitwise or: [x | y] *)

  val bitxor: Cil.ikind -> t -> t -> t
  (** Bitwise exclusive or: [x ^ y] *)

  val shift_left : Cil.ikind -> t -> t -> t
  (** Shifting bits left: [x << y] *)

  val shift_right: Cil.ikind -> t -> t -> t
  (** Shifting bits right: [x >> y] *)


  (** {b Logical operators} *)

  val lognot: Cil.ikind -> t -> t
  (** Logical not: [!x] *)

  val logand: Cil.ikind -> t -> t -> t
  (** Logical and: [x && y] *)

  val logor : Cil.ikind -> t -> t -> t
  (** Logical or: [x || y] *)

end

(* Shared signature of IntDomain implementations and the lifted IntDomains *)
module type B =
sig
  include Lattice.S
  type int_t
  (** {b Accessing values of the ADT} *)

  val bot_of: Cil.ikind -> t
  val top_of: Cil.ikind -> t

  val to_int: t -> int_t option
  (** Return a single integer value if the value is a known constant, otherwise
    * don't return anything. *)

  val equal_to: int_t -> t -> [`Eq | `Neq | `Top]

  val to_bool: t -> bool option
  (** Give a boolean interpretation of an abstract value if possible, otherwise
    * don't return anything.*)

  val to_excl_list: t -> (int_t list * (int64 * int64)) option
  (** Gives a list representation of the excluded values from included range of bits if possible. *)

  val of_excl_list: Cil.ikind -> int_t list -> t
  (** Creates an exclusion set from a given list of integers. *)

  val is_excl_list: t -> bool
  (** Checks if the element is an exclusion set. *)

  val to_incl_list: t -> int_t list option
  (** Gives a list representation of the included values if possible. *)

  val maximal    : t -> int_t option
  val minimal    : t -> int_t option

  (** {b Cast} *)

  val cast_to: ?torg:Cil.typ -> Cil.ikind -> t -> t
  (** Cast from original type [torg] to integer type [Cil.ikind]. Currently, [torg] is only present for actual casts. The function is also called to handle overflows/wrap around after operations. In these cases (where the type stays the same) [torg] is None. *)

end

(** The signature of integral value domains. They need to support all integer
  * operations that are allowed in C *)

module type IkindUnawareS =
sig
  include B
  include Arith with type t:= t
  val starting   : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val ending     : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val of_int: int_t -> t
  (** Transform an integer literal to your internal domain representation. *)

  val of_bool: bool -> t
  (** Transform a known boolean value to the default internal representation. It
    * should follow C: [of_bool true = of_int 1] and [of_bool false = of_int 0]. *)

  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t

  val of_congruence: Cil.ikind -> int_t * int_t -> t
  val arbitrary: unit -> t QCheck.arbitrary
  val invariant: Cil.exp -> t -> Invariant.t
end
(** Interface of IntDomain implementations that do not take ikinds for arithmetic operations yet.
   TODO: Should be ported to S in the future. *)

module type S =
sig
  include B
  include ArithIkind with type t:= t

  val add : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val sub : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val mul : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val div : ?no_ov:bool -> Cil.ikind ->  t -> t -> t
  val neg : ?no_ov:bool -> Cil.ikind ->  t -> t
  val cast_to : ?torg:Cil.typ -> ?no_ov:bool -> Cil.ikind -> t -> t
  (** @param no_ov If true, assume no overflow can occur. *)

  val join: Cil.ikind -> t ->  t -> t
  val meet: Cil.ikind -> t -> t -> t
  val narrow: Cil.ikind -> t -> t -> t
  val widen: Cil.ikind -> t -> t -> t
  val starting : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val ending : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val of_int: Cil.ikind -> int_t -> t
  (** Transform an integer literal to your internal domain representation. *)

  val of_bool: Cil.ikind -> bool -> t
  (** Transform a known boolean value to the default internal representation. It
    * should follow C: [of_bool true = of_int 1] and [of_bool false = of_int 0]. *)

  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t
  val of_congruence: Cil.ikind -> int_t * int_t -> t
  val is_top_of: Cil.ikind -> t -> bool
  val invariant_ikind : Cil.exp -> Cil.ikind -> t -> Invariant.t

  val refine_with_congruence: Cil.ikind -> t -> (int_t * int_t) option -> t
  val refine_with_interval: Cil.ikind -> t -> (int_t * int_t) option -> t
  val refine_with_excl_list: Cil.ikind -> t -> (int_t list * (int64 * int64)) option -> t
  val refine_with_incl_list: Cil.ikind -> t -> int_t list option -> t

  val project: Cil.ikind -> PrecisionUtil.int_precision -> t -> t
  val arbitrary: Cil.ikind -> t QCheck.arbitrary
end
(** Interface of IntDomain implementations taking an ikind for arithmetic operations *)

module type SOverFlow = 
sig

  include S

  val add : ?no_ov:bool -> Cil.ikind ->  t -> t -> t * bool * bool * bool

  val sub : ?no_ov:bool -> Cil.ikind ->  t -> t -> t * bool * bool * bool

  val mul : ?no_ov:bool -> Cil.ikind ->  t -> t -> t * bool * bool * bool

  val div : ?no_ov:bool -> Cil.ikind ->  t -> t -> t * bool * bool * bool

  val neg : ?no_ov:bool -> Cil.ikind ->  t -> t * bool * bool * bool
  
  val cast_to : ?torg:Cil.typ -> ?no_ov:bool -> Cil.ikind -> t -> t * bool * bool * bool

  val of_int : Cil.ikind -> int_t -> t * bool * bool * bool

  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t * bool  * bool * bool

  val starting : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t * bool  * bool * bool
  val ending : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t * bool * bool * bool

  val shift_left : Cil.ikind -> t -> t -> t * bool * bool * bool

  val shift_right: Cil.ikind -> t -> t -> t * bool * bool * bool


end

val unlift : 'a * bool * bool * bool -> 'a

module SOverFlowLifter (D : S) : SOverFlow with type int_t = D.int_t and type t = D.t

module SOverFlowUnlifter (D : SOverFlow) : S with type int_t = D.int_t and type t = D.t 

module OldDomainFacade (Old : IkindUnawareS with type int_t = int64) : S with type int_t = IntOps.BigIntOps.t and type t = Old.t
(** Facade for IntDomain implementations that do not implement the interface where arithmetic functions take an ikind parameter. *)

module type Y =
sig
  include B
  include Arith with type t:=t

  val of_int: Cil.ikind -> int_t -> t
  (** Transform an integer literal to your internal domain representation with the specified ikind. *)

  val of_bool: Cil.ikind -> bool -> t
  (** Transform a known boolean value to the default internal representation of the specified ikind. It
    * should follow C: [of_bool true = of_int 1] and [of_bool false = of_int 0]. *)

  val of_interval: ?suppress_ovwarn:bool -> Cil.ikind -> int_t * int_t -> t

  val of_congruence: Cil.ikind -> int_t * int_t -> t

  val starting   : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t
  val ending     : ?suppress_ovwarn:bool -> Cil.ikind -> int_t -> t

  val is_top_of: Cil.ikind -> t -> bool

  val project: PrecisionUtil.int_precision -> t -> t
  val invariant: Cil.exp -> t -> Invariant.t
end
(** The signature of integral value domains keeping track of ikind information *)

module type Z = Y with type int_t = IntOps.BigIntOps.t 

module IntDomLifter (I: S)
  : sig
  type int_t = I.int_t
  type t = { v : I.t; ikind : ikind }

  val ikind: t -> ikind

  val equal : t -> t -> PrecisionUtil.float_precision
  val compare : t -> t -> int
  val hash : t -> int
  val update_v : t -> I.t -> t
  val get_v : t -> I.t
  val check_ikinds : t -> t -> unit
  val lift : (ikind -> I.t -> I.t) -> t -> t
  val lift_logical : (ikind -> I.t -> I.t) -> t -> t
  val lift2 : (ikind -> I.t -> I.t -> I.t) -> t -> t -> t
  val lift2_cmp : (ikind -> I.t -> I.t -> I.t) -> t -> t -> t
  val bot_of : ikind -> t
  val bot : unit -> 'a
  val is_bot : t -> PrecisionUtil.float_precision
  val top_of : ikind -> t
  val top : unit -> 'a
  val is_top : t -> PrecisionUtil.float_precision
  val leq : t -> t -> PrecisionUtil.float_precision
  val join : t -> t -> t
  val meet : t -> t -> t
  val widen : t -> t -> t
  val narrow : t -> t -> t
  val show : t -> string
  val pretty : unit -> t -> Pretty.doc
  val pretty_diff : unit -> t * t -> Pretty.doc
  val printXml : 'a BatInnerIO.output -> t -> unit
  val name : unit -> string
  val to_yojson : t -> Yojson.Safe.t
  val invariant : exp -> t -> Invariant.t
  val tag : t -> int
  val arbitrary : 'a -> 'b
  val to_int : t -> I.int_t option
  val of_int : ikind -> I.int_t -> t
  val equal_to : I.int_t -> t -> [ `Eq | `Neq | `Top ]
  val to_bool : t -> PrecisionUtil.float_precision option
  val of_bool : ikind -> PrecisionUtil.float_precision -> t

  val to_excl_list :
    t -> (I.int_t list * (int64 * int64)) option

  val of_excl_list : ikind -> I.int_t list -> t
  val is_excl_list : t -> PrecisionUtil.float_precision
  val to_incl_list : t -> I.int_t list option
  val of_interval : ?suppress_ovwarn:bool -> ikind -> I.int_t * I.int_t -> t
  val of_congruence : ikind -> I.int_t * I.int_t -> t
  val starting : ?suppress_ovwarn:bool -> ikind -> I.int_t -> t
  val ending : ?suppress_ovwarn:bool -> ikind -> I.int_t -> t
  val maximal : t -> I.int_t option
  val minimal : t -> I.int_t option
  val neg : t -> t
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val rem : t -> t -> t
  val lt : t -> t -> t
  val gt : t -> t -> t
  val le : t -> t -> t
  val ge : t -> t -> t
  val eq : t -> t -> t
  val ne : t -> t -> t
  val bitnot : t -> t
  val bitand : t -> t -> t
  val bitor : t -> t -> t
  val bitxor : t -> t -> t
  val shift_left : t -> t -> t
  val shift_right : t -> t -> t
  val lognot : t -> t
  val logand : t -> t -> t
  val logor : t -> t -> t
  val cast_to : ?torg:'a -> ikind -> t -> t
  val is_top_of : ikind -> t -> PrecisionUtil.float_precision
  val relift : t -> t
  val project : PrecisionUtil.int_precision -> t -> t
end


module type Ikind =
sig
  val ikind: unit -> Cil.ikind
end

module PtrDiffIkind : Ikind

module IntDomWithDefaultIkind (I: Y) (Ik: Ikind) : Y with type t = I.t and type int_t = I.int_t

module Size : sig
  (** The biggest type we support for integers. *)
  val top_typ         : Cil.typ
  val range           : Cil.ikind -> Z.t * Z.t
  val is_cast_injective : from_type:Cil.typ -> to_type:Cil.typ -> bool
  val bits            : Cil.ikind -> int * int
  val cast            : Cil.ikind -> Z.t -> Z.t
  val min_from_bit_range : int64 -> Z.t
  val max_from_bit_range : int64 -> Z.t
end

module BISet: SetDomain.S with type elt = Z.t

exception ArithmeticOnIntegerBot of string

exception Unknown
(** An exception that can be raised when the result of a computation is unknown.
  * This is caught by lifted domains and will be replaced by top. *)

exception Error
(** An exception that can be raised when an arithmetic error occurs. This is
  * caught by lifted domains and the evaluation will then be set to bot, which
  * signifies an error in computation *)

exception IncompatibleIKinds of string

(** {b Predefined domains} *)
module Integers(Ints_t : IntOps.IntOps): IkindUnawareS with type t = Ints_t.t and type int_t = Ints_t.t
(** The integers with their natural orderings. Calling [top] and [bot] will
  * raise exceptions. *)

module FlatPureIntegers: IkindUnawareS with type t = IntOps.Int64Ops.t and type int_t = IntOps.Int64Ops.t
(** The integers with flattened orderings. Calling [top] and [bot] or [join]ing
    or [meet]ing inequal elements will raise exceptions. *)

module Flattened : IkindUnawareS with type t = [`Top | `Lifted of IntOps.Int64Ops.t | `Bot] and type int_t = IntOps.Int64Ops.t
(** This is the typical flattened integer domain used in Kildall's constant
  * propagation. *)

module FlattenedBI : IkindUnawareS with type t = [`Top | `Lifted of IntOps.BigIntOps.t | `Bot] and type int_t = IntOps.BigIntOps.t
(** This is the typical flattened integer domain used in Kildall's constant
  * propagation, using Big_int instead of int64. *)

module Lifted : IkindUnawareS with type t = [`Top | `Lifted of int64 | `Bot] and type int_t = int64
(** Artificially bounded integers in their natural ordering. *)

module Std 
  (B : sig
     type t

     val name : unit -> string
     val top_of : ikind -> t
     val bot_of : ikind -> t
     val show : t -> string
     val equal : t -> t -> PrecisionUtil.float_precision
   end)
  : sig
  type group = Invariant.group = |
  val show_group : group -> 'a
  val to_group : 'a -> 'b option
  val trace_enabled : PrecisionUtil.float_precision
  val tag : 'a -> 'b
  val arbitrary : unit -> 'a
  val relift : 'a -> 'a
  val name : unit -> string
  val is_top : 'a -> 'b
  val is_bot : B.t -> PrecisionUtil.float_precision
  val is_top_of : ikind -> B.t -> PrecisionUtil.float_precision
  val pretty : unit -> B.t -> Pretty.doc
  val to_yojson : B.t -> [> `String of string ]
  val printXml : 'a BatInnerIO.output -> B.t -> unit
  val pretty_diff : unit -> B.t * B.t -> Pretty.doc
  val to_excl_list : 'a -> 'b option
  val of_excl_list : ikind -> 'a -> B.t
  val is_excl_list : 'a -> PrecisionUtil.float_precision
  val to_incl_list : 'a -> 'b option
  val of_interval : ?suppress_ovwarn:bool -> ikind -> 'a -> B.t
  val of_congruence : ikind -> 'a -> B.t
  val starting : ?suppress_ovwarn:bool -> ikind -> 'a -> B.t
  val ending : ?suppress_ovwarn:bool -> ikind -> 'a -> B.t
  val maximal : 'a -> 'b option
  val minimal : 'a -> 'b option
end



module IntervalFunctor(Ints_t : IntOps.IntOps): SOverFlow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) option

module Interval32 :Y with (* type t = (IntOps.Int64Ops.t * IntOps.Int64Ops.t) option and *) type int_t = IntOps.Int64Ops.t

module BigInt:
  sig
    include Printable.S with type t = Z.t (* TODO: why doesn't this have a more useful signature like IntOps.BigIntOps? *)
    val cast_to: Cil.ikind -> Z.t -> Z.t
  end

module Interval : SOverFlow with type int_t = IntOps.BigIntOps.t

module Congruence : S with type int_t = IntOps.BigIntOps.t and type t = (IntOps.BigIntOps.t * IntOps.BigIntOps.t) option

module DefExc : S with type int_t = IntOps.BigIntOps.t
(** The DefExc domain. The Flattened integer domain is topped by exclusion sets.
  * Good for analysing branches. *)

(** {b Domain constructors} *)

module Flat (Base: IkindUnawareS): IkindUnawareS with type t = [ `Bot | `Lifted of Base.t | `Top ] and type int_t = Base.int_t
(** Creates a flat value domain, where all ordering is lost. Arithmetic
  * operations are lifted such that only lifted values can be evaluated
  * otherwise the top/bot is simply propagated with bot taking precedence over
  * top. *)

module Lift (Base: IkindUnawareS): IkindUnawareS with type t = [ `Bot | `Lifted of Base.t | `Top ] and type int_t = Base.int_t
(** Just like {!Value.Flat} except the order is preserved. *)

module Reverse (Base: IkindUnawareS): IkindUnawareS with type t = Base.t and type int_t = Base.int_t
(** Reverses bot, top, leq, join, meet *)

(* module Interval : S *)
(** Interval domain with int64-s --- use with caution! *)

(* module IncExcInterval : S with type t = [ | `Excluded of Interval.t| `Included of Interval.t ] *)
(** Inclusive and exclusive intervals. Warning: NOT A LATTICE *)
module Enums : S with type int_t = IntOps.BigIntOps.t

(** {b Boolean domains} *)

module type BooleansNames =
sig
  val truename: string
  (** The name of the [true] abstract value *)

  val falsename: string
  (** The name of the [false] abstract value *)
end
(** Parameter signature for the [MakeBooleans] functor. *)

module MakeBooleans (Names: BooleansNames): IkindUnawareS with type t = bool
(** Creates an abstract domain for integers represented by boolean values. *)

module Booleans: IkindUnawareS with type t = bool
(** Boolean abstract domain, where true is output "True" and false is output
  * "False" *)

(*
module None: S with type t = unit
(** Domain with nothing in it. *)
*)
