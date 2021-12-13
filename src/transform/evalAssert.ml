open Prelude
open Cil
open Formatcil
open Str

(** Instruments a program by inserting asserts:
      - After an assignment to a variable
      - At join points about all local variables

    Limitations:
      - Currently only works for top-level variables (not inside an array, a struct, ...)
      - Does not work for accesses through pointers
      - At join points asserts all locals, but ideally should only assert ones that are
        modified in one of the branches
      - Removes comments, so if the original program had //UNKNOWN assertions, the unknown
        will be removed and they will fail on the next iteration
*)
module EvalAssert = struct
  (* Cannot use Cilfacade.name_fundecs as assert() is external and has no fundec *)
  let ass = ref (makeVarinfo true "assert" (TVoid []))
  let locals = ref []
  class visitor (ask:Cil.location -> Queries.ask) = object(self)
    inherit nopCilVisitor

    method! vfunc f =
      locals := !locals @ f.slocals;
      DoChildren

    method! vglob g = match g with
      | GVarDecl (v, l) ->
        if v.vname = "assert" then begin
          ass := v;
          SkipChildren end
        else DoChildren
      | _ -> DoChildren

    method! vstmt s =
      let make_assert loc lval =
        try
          let res = (ask loc).f (Queries.Assert (Lval lval)) in
          if Queries.ES.is_bot res then
            []
          else
            let e = Queries.ES.choose res in
            [cInstr ("%v:assert (%e:exp);") loc [("assert", Fv !ass); ("exp", Fe e)]]
        with
          Not_found -> []
      in

      let rec instrument_instructions il s = match il with
        | i1 :: i2 :: is ->
          begin
            match i1 with
            | Set (lval, _, _, _)
            | Call (Some lval, _, _, _, _) -> [i1] @ (make_assert (get_instrLoc i2) lval) @ instrument_instructions (i2 :: is) s
            | _ -> i1 :: instrument_instructions (i2 :: is) s
          end
        | [i] ->
          if List.length s.succs > 0 && (List.hd s.succs).preds |> List.length < 2 then begin
            (* If the successor of this has more than one predecessor, it is a join point, and we can not query for the value there *)
            let loc = get_stmtLoc (List.hd s.succs).skind in
            match i with
            | Set (lval, _, _, _)
            | Call (Some lval, _, _, _, _) -> [i] @ (make_assert loc lval)
            | _ -> [i]
          end
          else [i]
        | _ -> []
      in

      let rec get_vars e =
        match e with
        | Lval (Var v, _) -> [v]
        | UnOp (_, e, _) -> get_vars e
        | BinOp (_, e1, e2, _) -> (get_vars e1) @ (get_vars e2)
        | _ -> []
      in

      let instrument_join s =
        match s.preds with
        | [p1; p2] ->
          (* exactly two predecessors -> join point, assert locals if they changed *)
          (* Possible enhancement: It would be nice to only assert locals here that were modified in either branch *)
          let join_loc = get_stmtLoc s.skind in
          let asserts = List.map (fun x -> make_assert join_loc (Var x,NoOffset)) !locals |> List.concat in
          self#queueInstr asserts; ()
        | _ -> ()
      in

      let instrument_statement s =
        instrument_join s;
        match s.skind with
        | Instr il ->
          s.skind <- Instr (instrument_instructions il s);
          s
        | If (e, b1, b2, l,l2) ->
          let vars = get_vars e in
          let asserts loc vs = List.map (fun x -> make_assert loc (Var x,NoOffset)) vs |> List.concat in
          let add_asserts block =
            if List.length block.bstmts > 0 then
              let with_asserts =
                let b_loc = get_stmtLoc (List.hd block.bstmts).skind in
                let b_assert_instr = asserts b_loc vars in
                [cStmt "{ %I:asserts %S:b }" (fun n t -> makeVarinfo true "unknown" (TVoid [])) b_loc [("asserts", FI b_assert_instr); ("b", FS block.bstmts)]]
              in
              block.bstmts <- with_asserts
            else
              ()
          in

          add_asserts b1;
          add_asserts b2;
          s
        | _ -> s
      in
      ChangeDoChildrenPost (s, instrument_statement)
  end
  let transform (ask:Cil.location -> Queries.ask) file = begin
    visitCilFile (new visitor ask) file;
    let assert_filename = "annotated.c" in
    let oc = Stdlib.open_out assert_filename in
    dumpFile defaultCilPrinter oc assert_filename file; end
end
let _ = Transform.register "assert" (module EvalAssert)
