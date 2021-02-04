open Prelude

(** Thread ID *)
module Tid = IntDomain.Flattened

(** Context hash for function calls *)
module Ctx = IntDomain.Flattened

(** Set of predecessor nodes *)
module Pred = struct
  module Base = Basetype.ProgLocation
  include SetDomain.Make (Base)

  let of_node = singleton % MyCFG.getLoc

  let of_current_node () = of_node @@ Option.get !MyCFG.current_node

  let string_of_elt = Basetype.ProgLocation.short 99
end

type domain =
  { tid : Tid.t
  ; pred : Pred.t
  ; ctx : Ctx.t
  }
[@@deriving to_yojson]

module D = struct
  include Printable.Std

  type t = domain [@@deriving to_yojson]

  (** printing *)
  let short w x =
    Printf.sprintf
      "{ Tid=%s; pred=%s; ctx=%s }"
      (Tid.short 3 x.tid)
      (Pretty.sprint 200 (Pred.pretty () x.pred))
      (Ctx.short 50 x.ctx)


  include Printable.PrintSimple (struct
    type t' = t

    let name () = "pthread state"

    let short = short
  end)

  (** let equal = Util.equals *)
  let equal x y =
    Tid.equal x.tid y.tid && Pred.equal x.pred y.pred && Ctx.equal x.ctx y.ctx


  (** compare all fields with correspoding compare operators *)
  let compare x y =
    List.fold_left
      (fun acc v -> if acc = 0 && v <> 0 then v else acc)
      0
      [ Tid.compare x.tid y.tid
      ; Pred.compare x.pred y.pred
      ; Ctx.compare x.ctx y.ctx
      ]


  (** let hash = Hashtbl.hash *)
  let hash x = Hashtbl.hash (Tid.hash x.tid, Pred.hash x.pred, Ctx.hash x.ctx)

  let make tid pred ctx = { tid; pred; ctx }

  let bot () = { tid = Tid.bot (); pred = Pred.bot (); ctx = Ctx.bot () }

  let is_bot x = x = bot ()

  let any_is_bot x = Tid.is_bot x.tid || Pred.is_bot x.pred

  let top () = { tid = Tid.top (); pred = Pred.top (); ctx = Ctx.top () }

  let is_top x = Tid.is_top x.tid && Pred.is_top x.pred && Ctx.is_top x.ctx

  let op_scheme op1 op2 op3 x y : t =
    { tid = op1 x.tid y.tid; pred = op2 x.pred y.pred; ctx = op3 x.ctx y.ctx }


  let leq x y =
    Tid.leq x.tid y.tid && Pred.leq x.pred y.pred && Ctx.leq x.ctx y.ctx


  let join = op_scheme Tid.join Pred.join Ctx.join

  let widen = join

  let meet = op_scheme Tid.meet Pred.meet Ctx.meet

  let narrow = meet
end
