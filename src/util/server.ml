open Batteries
open Jsonrpc

exception Failure of Response.Error.Code.t * string

type t = {
  mutable file: Cil.file;
  mutable version_map: (CompareCIL.global_identifier, Cil.global) Hashtbl.t;
  mutable max_ids: VersionLookup.max_ids;
  preprocess_and_merge: unit -> Cil.file;
  do_analyze: Analyses.increment_data -> Cil.file -> unit;
  input: IO.input;
  output: unit IO.output;
}

module type Request = sig
  val name: string

  type params
  type response

  val params_of_yojson: Yojson.Safe.t -> (params, string) result
  val response_to_yojson: response -> Yojson.Safe.t

  val process: params -> t -> response
end

module Registry = struct
  type t = (string, (module Request)) Hashtbl.t
  let make () : t = Hashtbl.create 32
  let register (reg: t) (module R : Request) = Hashtbl.add reg R.name (module R)
end

let registry = Registry.make ()

let handle_exn id exn =
  Response.Error.(make ~code:Code.InternalError ~message:(Printexc.to_string exn) () |> Response.error id)

module ParamParser (R : Request) = struct
  let parse params =
    let maybe_params =
      params
      |> Option.map_default Message.Structured.to_json `Null
      |> R.params_of_yojson
    in
    match maybe_params with
    | Ok params -> Ok params
    | Error err ->
      (* This is a hack to handle cases where R.params is a primitive type like int or string. *)
      match params with
      | Some `List [param] -> R.params_of_yojson param |> Result.map_error (fun _ -> err)
      | _ -> Error err
end

let handle_request (serv: t) (message: Message.either) (id: Id.t) =
  let req = Hashtbl.find_option registry message.method_ in
  let response = match req with
    | Some (module R) ->
      let module Parser = ParamParser (R) in (
        match Parser.parse message.params with
        | Ok params -> (
            try
              R.process params serv
              |> R.response_to_yojson
              |> Response.ok id
            with Failure (code, message) -> Response.Error.(make ~code ~message () |> Response.error id))
        | Error message -> Response.Error.(make ~code:Code.InvalidParams ~message () |> Response.error id))
    | _ -> Response.Error.(make ~code:Code.MethodNotFound ~message:message.method_ () |> Response.error id)
  in
  Response.yojson_of_t response |> Yojson.Safe.to_string |> IO.write_line serv.output;
  IO.flush serv.output

let serve serv =
  try
    while true; do
      let line = IO.read_line serv.input in
      try
        let json = Yojson.Safe.from_string line in
        let message = Message.either_of_yojson json in
        match message.id with
        | Some id -> handle_request serv message id
        | _ -> () (* We just ignore notifications for now. *)
      with Yojson.Json_error s | Json.Of_json (s, _) -> prerr_endline s
    done
  with IO.No_more_input -> ()

let make ?(input=stdin) ?(output=stdout) file preprocess_and_merge do_analyze : t =
  let version_map, max_ids = VersionLookup.create_map file in
  {
    file;
    version_map;
    max_ids;
    preprocess_and_merge;
    do_analyze;
    input;
    output
  }

let bind () =
  let mode = GobConfig.get_string "server.mode" in
  if mode = "stdio" then None, None else (
    let path = GobConfig.get_string "server.unix-socket" in
    if Sys.file_exists path then
      Sys.remove path;
    let socket = Unix.socket PF_UNIX SOCK_STREAM 0 in
    Unix.bind socket (ADDR_UNIX path);
    Unix.listen socket 1;
    let conn, _ = Unix.accept socket in
    Unix.close socket;
    Sys.remove path;
    Some (Unix.input_of_descr conn), Some (Unix.output_of_descr conn))

let start file preprocess_and_merge do_analyze =
  let input, output = bind () in
  GobConfig.set_bool "incremental.save" true;
  serve (make file preprocess_and_merge do_analyze ?input ?output)

let reparse (s: t) =
  if GobConfig.get_bool "server.reparse" then (
    Goblintutil.create_temp_dir ();
    Fun.protect ~finally:Goblintutil.remove_temp_dir s.preprocess_and_merge, true)
  else s.file, false

(* Only called when the file has not been reparsed, so we can skip the expensive CFG comparison. *)
let virtual_changes file =
  let changes = CompareCIL.empty_change_info () in
  let reanalyze = GobConfig.get_string_list "incremental.force-reanalyze.funs" in
  Cil.iterGlobals file (
    function
    | GFun (fundec, _) as global ->
      if List.mem fundec.svar.vname reanalyze then
        changes.changed <- { old = global; current = global; unchangedHeader = true; diff = None } :: changes.changed
      else
        changes.unchanged <- global :: changes.unchanged
    | global -> changes.unchanged <- global :: changes.unchanged);
  changes

let increment_data (s: t) file reparsed = match !Serialize.server_solver_data with
  | Some solver_data when reparsed ->
    let _, changes = VersionLookup.updateMap s.file file s.version_map in
    let old_data = Some { Analyses.cil_file = s.file; solver_data } in
    s.max_ids <- UpdateCil.update_ids s.file s.max_ids file s.version_map changes;
    { Analyses.changes; old_data; new_file = file }, false
  | Some solver_data ->
    let changes = virtual_changes file in
    let old_data = Some { Analyses.cil_file = file; solver_data } in
    { Analyses.changes; old_data; new_file = file }, false
  | _ -> Analyses.empty_increment_data file, true

let analyze ?(reset=false) (s: t) =
  let file, reparsed = reparse s in
  if reset then (
    let version_map, max_ids = VersionLookup.create_map file in
    s.version_map <- version_map;
    s.max_ids <- max_ids;
    Serialize.server_solver_data := None;
    Serialize.server_analysis_data := None;
    Messages.Table.(MH.clear messages_table);
    Messages.Table.messages_list := []);
  let increment_data, fresh = increment_data s file reparsed in
  Cilfacade.reset_lazy ();
  WideningThresholds.reset_lazy ();
  IntDomain.reset_lazy ();
  ApronDomain.reset_lazy ();
  s.file <- file;
  GobConfig.set_bool "incremental.load" (not fresh);
  s.do_analyze increment_data s.file;
  GobConfig.set_bool "incremental.load" true

(* TODO: Add command to abort the analysis in progress. *)
let () =
  let register = Registry.register registry in

  register (module struct
    let name = "analyze"
    type params = { reset: bool [@default false] } [@@deriving of_yojson]
    (* TODO: Return analysis results as JSON. Useful for GobPie. *)
    type response = unit [@@deriving to_yojson]
    (* TODO: Add options to control the analysis precision/context for specific functions. *)
    (* TODO: Add option to mark functions as modified. *)
    let process { reset } serve = analyze serve ~reset
  end);

  register (module struct
    let name = "config"
    type params = string * Yojson.Safe.t [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    (* TODO: Make it possible to change the non-optional parameters. (i.e., the set of input files) *)
    (* TODO: Check options for compatibility with the incremental analysis. *)
    let process (conf, json) _ =
      try
        GobConfig.set_auto conf (Yojson.Safe.to_string json)
      with exn -> raise (Failure (InvalidParams, Printexc.to_string exn))
  end);

  register (module struct
    let name = "merge_config"
    type params = Yojson.Safe.t [@@deriving of_yojson]
    type response = unit [@@deriving to_yojson]
    let process json _ =
      try GobConfig.merge json with exn -> (* TODO: Be more specific in what we catch. *)
        raise (Failure (InvalidParams, Printexc.to_string exn))
  end);

  register (module struct
    let name = "messages"
    type params = unit [@@deriving of_yojson]
    type response = Messages.Message.t list [@@deriving to_yojson]
    let process () _ = !Messages.Table.messages_list
  end);

  register (module struct
    let name = "exp_eval"
    type params = ExpressionEvaluation.query [@@deriving of_yojson]
    type response =
      ((string * CilType.Location.t * string * int) * bool option) list [@@deriving to_yojson]
    let process query serv =
      GobConfig.set_auto "trans.activated[+]" "'expeval'";
      ExpressionEvaluation.gv_query := Some query;
      analyze serv;
      GobConfig.set_auto "trans.activated[-]" "'expeval'";
      !ExpressionEvaluation.gv_results
  end);

  register (module struct
    let name = "ping"
    type params = unit [@@deriving of_yojson]
    type response = [`Pong] [@@deriving to_yojson]
    let process () _ = `Pong
  end)
