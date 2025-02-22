(** Path-sensitive analysis that verifies checking the result of the malloc function. *)

module AD = ValueDomain.AD
module IdxDom = ValueDomain.IndexDomain
module Offs = ValueDomain.Offs

open Prelude.Ana
open Analyses

module Spec =
struct
  include Analyses.DefaultSpec

  module Addr = ValueDomain.Addr
  module D = ValueDomain.AddrSetDomain
  module C = ValueDomain.AddrSetDomain

  let should_join x y = D.equal x y

  (* NB! Currently we care only about concrete indexes. Base (seeing only a int domain
     element) answers with the string "unknown" on all non-concrete cases. *)
  let rec conv_offset x =
    match x with
    | `NoOffset    -> `NoOffset
    | `Index (Const (CInt (i,ik,s)),o) -> `Index (IntDomain.of_const (i,ik,s), conv_offset o)
    | `Index (_,o) -> `Index (IdxDom.top (), conv_offset o)
    | `Field (f,o) -> `Field (f, conv_offset o)

  (*
    Addr set functions:
  *)
  let is_prefix_of (v1,ofs1: varinfo * (Addr.field,Addr.idx) Lval.offs) (v2,ofs2: varinfo * (Addr.field,Addr.idx) Lval.offs) : bool =
    let rec is_offs_prefix_of pr os =
      match (pr, os) with
      | (`NoOffset, `NoOffset) -> true
      | (`NoOffset, _) -> false
      | (`Field (f1, o1), `Field (f2,o2)) -> f1 == f2 && is_offs_prefix_of o1 o2
      | (_, _) -> false
    in
    CilType.Varinfo.equal v1 v2 && is_offs_prefix_of ofs1 ofs2

  (* We just had to dereference an lval --- warn if it was null *)
  let warn_lval (st:D.t) (v :varinfo * (Addr.field,Addr.idx) Lval.offs) : unit =
    try
      if D.exists (fun x -> GobOption.exists (fun x -> is_prefix_of x v) (Addr.to_var_offset x)) st
      then
        let var = Addr.from_var_offset v in
        Messages.warn ~category:Messages.Category.Behavior.Undefined.nullpointer_dereference "Possible dereferencing of null on variable '%a'." Addr.pretty var
    with SetDomain.Unsupported _ -> ()

  (* Warn null-lval dereferences, but not normal (null-) lvals*)
  let rec warn_deref_exp (a: Queries.ask) (st:D.t) (e:exp): unit =
    let warn_lval_mem e offs =
      (*      begin try List.iter (warn_lval st) (AD.to_var_offset (BS.eval_lv gl s (Mem e, offs)))
              with SetDomain.Unsupported _ -> () end;*)
      match e with
      | Lval (Var v, offs) ->
        begin match a.f (Queries.MayPointTo (mkAddrOf (Var v,offs))) with
          | a when not (Queries.LS.is_top a)
                         && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
            Queries.LS.iter (fun (v,o) -> warn_lval st (v, conv_offset o)) a
          | _ -> ()
        end
      | _ -> ()
    in
    match e with
    | Const _
    | SizeOf _
    | SizeOfStr _
    | AlignOf _
    | AddrOfLabel _
    | Lval (Var _, _) -> ()
    | AddrOf (Var _, _)
    | StartOf (Var _, _) ->  warn_lval_mem e NoOffset
    | AddrOf (Mem e, offs)
    | StartOf (Mem e, offs)
    | Lval (Mem e, offs) ->
      warn_deref_exp a st e;
      warn_lval_mem e offs
    | BinOp (_,e1,e2,_) ->
      warn_deref_exp a st e1;
      warn_deref_exp a st e2
    | UnOp (_,e,_)
    | Real e
    | Imag e
    | SizeOfE e
    | AlignOfE e
    | CastE  (_,e) ->
      warn_deref_exp a st e
    | Question (b, t, f, _) ->
      warn_deref_exp a st b;
      warn_deref_exp a st t;
      warn_deref_exp a st f

  let may (f: 'a -> 'b) (x: 'a option) : unit =
    match x with
    | Some x -> f x
    | None -> ()

  (* Generate addresses to all points in an given varinfo. (Depends on type) *)
  let to_addrs (v:varinfo) : Addr.t list =
    let make_offs = List.fold_left (fun o f -> `Field (f, o)) `NoOffset in
    let rec add_fields (base: Addr.field list) fs acc =
      match fs with
      | [] -> acc
      | f :: fs ->
        match unrollType f.ftype with
        | TComp ({cfields=ffs; _},_) -> add_fields base fs (List.rev_append (add_fields (f::base) ffs []) acc)
        | _                       -> add_fields base fs ((Addr.from_var_offset (v,make_offs (f::base))) :: acc)
    in
    match unrollType v.vtype with
    | TComp ({cfields=fs; _},_) -> add_fields [] fs []
    | _ -> [Addr.from_var v]

  (* Remove null values from state that are unreachable from exp.*)
  let remove_unreachable (ask: Queries.ask) (args: exp list) (st: D.t) : D.t =
    let reachable =
      let do_exp e =
        match ask.f (Queries.ReachableFrom e) with
        | a when not (Queries.LS.is_top a)  ->
          let to_extra (v,o) xs = AD.from_var_offset (v,(conv_offset o)) :: xs  in
          Queries.LS.fold to_extra (Queries.LS.remove (dummyFunDec.svar, `NoOffset) a) []
        (* Ignore soundness warnings, as invalidation proper will raise them. *)
        | _ -> []
      in
      List.concat_map do_exp args
    in
    let add_exploded_struct (one: AD.t) (many: AD.t) : AD.t =
      let vars = AD.to_var_may one in
      List.fold_right AD.add (List.concat_map to_addrs vars) many
    in
    let vars = List.fold_right add_exploded_struct reachable (AD.empty ()) in
    if D.is_top st
    then D.top ()
    else D.filter (fun x -> AD.mem x vars) st

  let get_concrete_lval (ask: Queries.ask) (lval:lval) =
    match ask.f (Queries.MayPointTo (mkAddrOf lval)) with
    | a when Queries.LS.cardinal a = 1
                   && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
      let v, o = Queries.LS.choose a in
      Some (Var v, conv_offset o)
    | _ -> None

  let get_concrete_exp (exp:exp) gl (st:D.t) =
    match constFold true exp with
    | CastE (_,Lval (Var v, offs))
    | Lval (Var v, offs) -> Some (Var v,offs)
    | _ -> None

  let might_be_null (ask: Queries.ask) lv gl st =
    match ask.f (Queries.MayPointTo (mkAddrOf lv)) with
    | a when not (Queries.LS.is_top a) && not (Queries.LS.mem (dummyFunDec.svar,`NoOffset) a) ->
      let one_addr_might (v,o) =
        D.exists (fun x -> GobOption.exists (fun x -> is_prefix_of (v, conv_offset o) x) (Addr.to_var_offset x)) st
      in
      Queries.LS.exists one_addr_might a
    | _ -> false

  (*
    Transfer functions and alike
  *)

  (* One step tf-s *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local (Lval lval) ;
    warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local rval;
    match get_concrete_exp rval ctx.global ctx.local, get_concrete_lval (Analyses.ask_of_ctx ctx) lval with
    | Some rv , Some (Var vt,ot) when might_be_null (Analyses.ask_of_ctx ctx) rv ctx.global ctx.local ->
      D.add (Addr.from_var_offset (vt,ot)) ctx.local
    | _ -> ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t =
    warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local exp;
    ctx.local

  let body ctx (f:fundec) : D.t =
    ctx.local

  let return_addr_ = ref Addr.NullPtr
  let return_addr () = !return_addr_

  let return ctx (exp:exp option) (f:fundec) : D.t =
    let remove_var x v = List.fold_right D.remove (to_addrs v) x in
    let nst = List.fold_left remove_var ctx.local (f.slocals @ f.sformals) in
    match exp with
    | Some ret ->
      warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local ret;
      begin match get_concrete_exp ret ctx.global ctx.local with
        | Some ev when might_be_null (Analyses.ask_of_ctx ctx) ev ctx.global ctx.local ->
          D.add (return_addr ()) nst
        | _ -> nst  end
    | None -> nst

  (* Function calls *)

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    let nst = remove_unreachable (Analyses.ask_of_ctx ctx) args ctx.local in
    may (fun x -> warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local (Lval x)) lval;
    List.iter (warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local) args;
    [ctx.local,nst]

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) (f_ask: Queries.ask) : D.t =
    let cal_st = remove_unreachable (Analyses.ask_of_ctx ctx) args ctx.local in
    let ret_st = D.union au (D.diff ctx.local cal_st) in
    let new_u =
      match lval, D.mem (return_addr ()) ret_st with
      | Some lv, true ->
        begin match get_concrete_lval (Analyses.ask_of_ctx ctx) lv with
          | Some (Var v,ofs) -> D.remove (return_addr ()) (D.add (Addr.from_var_offset (v,ofs)) ret_st)
          | _ -> ret_st end
      | _ -> ret_st
    in
    new_u

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    may (fun x -> warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local (Lval x)) lval;
    List.iter (warn_deref_exp (Analyses.ask_of_ctx ctx) ctx.local) arglist;
    let desc = LibraryFunctions.find f in
    match desc.special arglist, lval with
    | Malloc _, Some lv ->
      begin
        match get_concrete_lval (Analyses.ask_of_ctx ctx) lv with
        | Some (Var v, offs) ->
          ctx.split ctx.local [Events.SplitBranch ((Lval lv), true)];
          ctx.split (D.add (Addr.from_var_offset (v,offs)) ctx.local) [Events.SplitBranch ((Lval lv), false)];
          raise Analyses.Deadcode
        | _ -> ctx.local
      end
    | _ -> ctx.local

  let name () = "malloc_null"

  let startstate v = D.empty ()
  let threadenter ctx lval f args = [D.empty ()]
  let threadspawn ctx lval f args fctx = ctx.local
  let exitstate  v = D.empty ()

  let init marshal =
    return_addr_ :=  Addr.from_var (Goblintutil.create_var @@ makeVarinfo false "RETURN" voidType)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
