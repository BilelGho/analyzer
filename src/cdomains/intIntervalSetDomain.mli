module IntervalSetFunctor(Ints_t : IntOps.IntOps): IntDomain.SOverFlow with type int_t = Ints_t.t and type t = (Ints_t.t * Ints_t.t) list
