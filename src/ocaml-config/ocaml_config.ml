open! Stdune
open Result.O

(* Explicitly opened Unix at the top to avoid conflicts *)
module Unix_ops = Unix

module Prog_and_args = struct
  type t =
    { prog : string
    ; args : string list
    }
end

open Prog_and_args

module Value = struct
  type t =
    | Bool of bool
    | Int of int
    | String of string
    | Words of string list
    | Prog_and_args of Prog_and_args.t

  let to_dyn : t -> Dyn.t =
    let open Dyn in
    function
    | Bool x -> Bool x
    | Int x -> Int x
    | String x -> String x
    | Words x -> (list string) x
    | Prog_and_args { prog; args } -> (list string) (prog :: args)
  ;;

  let to_string = function
    | Bool x -> string_of_bool x
    | Int x -> string_of_int x
    | String x -> x
    | Words x -> String.concat x ~sep:" "
    | Prog_and_args x -> String.concat ~sep:" " (x.prog :: x.args)
  ;;
end

module Os_type = struct
  type t =
    | Win32
    | Unix
    | Other of string

  let of_string = function
    | "Win32" -> Win32
    | "Unix" -> Unix
    | s -> Other s
  ;;

  let to_string = function
    | Win32 -> "Win32"
    | Unix -> "Unix"
    | Other s -> s
  ;;
end

module Ccomp_type = struct
  type t =
    | Msvc
    | Cc
    | Other of string

  let to_dyn =
    let open Dyn in
    function
    | Msvc -> variant "Msvc" []
    | Cc -> variant "Cc" []
    | Other s -> variant "Other" [ string s ]
  ;;

  let of_string = function
    | "msvc" -> Msvc
    | "cc" -> Cc
    | s -> Other s
  ;;

  let to_string = function
    | Msvc -> "msvc"
    | Cc -> "cc"
    | Other s -> s
  ;;
end

module Origin = struct
  type t =
    | Ocamlc_config
    | Makefile_config of Path.t
end

type t = { ocamlc_path : string }

let split_prog s =
  match String.extract_blank_separated_words s with
  | [] -> None
  | prog :: args -> Some { prog; args }
;;

module Vars = struct
  type t = string String.Map.t

  let to_list = String.Map.to_list
  let of_list_exn = String.Map.of_list_exn
  let find = String.Map.find

  let of_lines lines =
    let rec loop acc = function
      | [] -> Ok acc
      | line :: lines ->
        (match String.index line ':' with
         | Some i ->
           let x =
             String.take line i, String.drop line (i + 2)
             (* skipping the space *)
           in
           loop (x :: acc) lines
         | None -> Error (Printf.sprintf "Unrecognized line: %S" line))
    in
    let* vars = loop [] lines in
    Result.map_error (String.Map.of_list vars) ~f:(fun (var, _, _) ->
      Printf.sprintf "Variable %S present twice." var)
  ;;

  exception E of Origin.t * string

  module Getters (Origin_arg : sig
      val origin : Origin.t
    end) =
  struct
    let fail fmt = Printf.ksprintf (fun msg -> raise (E (Origin_arg.origin, msg))) fmt
    let get_opt t var = String.Map.find t var

    let get t var =
      match get_opt t var with
      | Some s -> s
      | None -> fail "Variable %S not found." var
    ;;

    let get_bool t ?(default = false) var =
      match get_opt t var with
      | None -> default
      | Some s ->
        (match s with
         | "true" -> true
         | "false" -> false
         | s -> fail "Value of %S is neither 'true' neither 'false': %s." var s)
    ;;

    let get_int_opt t var =
      Option.bind (get_opt t var) ~f:(fun s ->
        match Int.of_string s with
        | Some x -> Some x
        | None -> fail "Value of %S is not an integer: %s." var s)
    ;;

    let get_words t var =
      match get_opt t var with
      | None -> []
      | Some s -> String.extract_blank_separated_words s
    ;;

    let get_prog_or_dummy t var =
      Option.map (get_opt t var) ~f:(fun v ->
        match split_prog v with
        | None -> { prog = Printf.sprintf "%s-not-found-in-ocaml-config" var; args = [] }
        | Some s -> s)
    ;;

    let get_prog_or_dummy_exn t var =
      match get_prog_or_dummy t var with
      | None -> fail "Variable %S not found." var
      | Some s -> s
    ;;
  end

  module Ocamlc_config_getters = Getters (struct
      let origin = Origin.Ocamlc_config
    end)
end

(*
   NEW: On-demand ocamlc -config runner using existing Vars infrastructure
*)

let run_ocamlc_config_and_parse ocamlc_path field_name =
  (* Log which field is being accessed *)
  let log_file = "/tmp/dune_field_access.log" in
  let oc = open_out_gen [ Open_creat; Open_append ] 0o644 log_file in
  Printf.fprintf oc "%s\n" field_name;
  flush oc;
  (*  ensure log writes complete *)
  close_out oc;
  (* Running ocamlc -config and saving to output file *)
  let temp_file =
    "/tmp/ocamlc_temp_output_" ^ string_of_int (Random.int 10000) ^ ".txt"
  in
  (* FIXED: unique temp files *)
  let out =
    Unix_ops.openfile
      temp_file
      [ Unix_ops.O_RDWR; Unix_ops.O_CREAT; Unix_ops.O_TRUNC ]
      0o644
  in
  let pid =
    Unix_ops.create_process
      ocamlc_path
      [| ocamlc_path; "-config" |]
      Unix_ops.stdin
      out
      Unix_ops.stderr
  in
  let _ = Unix_ops.waitpid [] pid in
  Unix_ops.close out;
  let ic = open_in temp_file in
  let lines = In_channel.input_lines ic in
  In_channel.close ic;
  (try Sys.remove temp_file with
   | _ -> ());
  match Vars.of_lines lines with
  | Ok vars -> vars
  | Error msg -> failwith ("Failed to parse ocamlc -config: " ^ msg)
;;

(*
   All getter functions now use on-demand loading with helpers functions
*)

let version t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "version" in
  let open Vars.Ocamlc_config_getters in
  let version_string = get vars "version" in
  match Scanf.sscanf version_string "%u.%u.%u" (fun a b c -> a, b, c) with
  | Ok tuple -> tuple
  | Error () -> failwith ("Unable to parse version: " ^ version_string)
;;

let version_string t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "version" in
  let open Vars.Ocamlc_config_getters in
  get vars "version"
;;

let standard_library_default t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "standard_library_default" in
  let open Vars.Ocamlc_config_getters in
  get vars "standard_library_default"
;;

let standard_library t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "standard_library" in
  let open Vars.Ocamlc_config_getters in
  get vars "standard_library"
;;

let standard_runtime t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "standard_runtime" in
  let open Vars.Ocamlc_config_getters in
  match get_opt vars "standard_runtime" with
  | Some value -> value
  | None -> "the_standard_runtime_variable_was_deleted"
;;

let ccomp_type t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ccomp_type" in
  let open Vars.Ocamlc_config_getters in
  Ccomp_type.of_string (get vars "ccomp_type")
;;

let c_compiler t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "c_compiler" in
  let open Vars.Ocamlc_config_getters in
  get vars "c_compiler"
;;

let ocamlc_cflags t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ocamlc_cflags" in
  let open Vars.Ocamlc_config_getters in
  get_words vars "ocamlc_cflags"
;;

let ocamlc_cppflags t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ocamlc_cppflags" in
  let open Vars.Ocamlc_config_getters in
  get_words vars "ocamlc_cppflags"
;;

let ocamlopt_cflags t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ocamlopt_cflags" in
  let open Vars.Ocamlc_config_getters in
  get_words vars "ocamlopt_cflags"
;;

let ocamlopt_cppflags t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ocamlopt_cppflags" in
  let open Vars.Ocamlc_config_getters in
  get_words vars "ocamlopt_cppflags"
;;

let bytecomp_c_compiler t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "bytecomp_c_compiler" in
  let open Vars.Ocamlc_config_getters in
  get_prog_or_dummy_exn vars "bytecomp_c_compiler"
;;

let bytecomp_c_libraries t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "bytecomp_c_libraries" in
  let open Vars.Ocamlc_config_getters in
  get_words vars "bytecomp_c_libraries"
;;

let native_c_compiler t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "native_c_compiler" in
  let open Vars.Ocamlc_config_getters in
  get_prog_or_dummy_exn vars "native_c_compiler"
;;

let native_c_libraries t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "native_c_libraries" in
  let open Vars.Ocamlc_config_getters in
  get_words vars "native_c_libraries"
;;

let native_pack_linker t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "native_pack_linker" in
  let open Vars.Ocamlc_config_getters in
  get_prog_or_dummy_exn vars "native_pack_linker"
;;

let cc_profile t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "cc_profile" in
  let open Vars.Ocamlc_config_getters in
  get_words vars "cc_profile"
;;

let architecture t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "architecture" in
  let open Vars.Ocamlc_config_getters in
  get vars "architecture"
;;

let model t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "model" in
  let open Vars.Ocamlc_config_getters in
  get vars "model"
;;

let int_size t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "int_size" in
  let open Vars.Ocamlc_config_getters in
  (* SIMPLIFIED: Removed get_arch_sixtyfour fallback logic *)
  match get_int_opt vars "int_size" with
  | Some n -> n
  | None -> 63 (* Default fallback *)
;;

let word_size t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "word_size" in
  let open Vars.Ocamlc_config_getters in
  (* SIMPLIFIED: Removed get_arch_sixtyfour fallback logic *)
  match get_int_opt vars "word_size" with
  | Some n -> n
  | None -> 64 (* Default fallback *)
;;

let system t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "system" in
  let open Vars.Ocamlc_config_getters in
  get vars "system"
;;

let asm t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "asm" in
  let open Vars.Ocamlc_config_getters in
  get_prog_or_dummy_exn vars "asm"
;;

let asm_cfi_supported t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "asm_cfi_supported" in
  let open Vars.Ocamlc_config_getters in
  get_bool vars "asm_cfi_supported"
;;

let with_frame_pointers t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "with_frame_pointers" in
  let open Vars.Ocamlc_config_getters in
  get_bool vars "with_frame_pointers"
;;

let ext_exe t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ext_exe" in
  let open Vars.Ocamlc_config_getters in
  match get_opt vars "exe_ext" with
  | Some s -> s
  | None ->
    let os_type_str = get vars "os_type" in
    if String.equal os_type_str "Win32" then ".exe" else ""
;;

let ext_obj t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ext_obj" in
  let open Vars.Ocamlc_config_getters in
  get vars "ext_obj"
;;

let ext_asm t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ext_asm" in
  let open Vars.Ocamlc_config_getters in
  get vars "ext_asm"
;;

let ext_lib t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ext_lib" in
  let open Vars.Ocamlc_config_getters in
  get vars "ext_lib"
;;

let ext_dll t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ext_dll" in
  let open Vars.Ocamlc_config_getters in
  get vars "ext_dll"
;;

let os_type t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "os_type" in
  let open Vars.Ocamlc_config_getters in
  Os_type.of_string (get vars "os_type")
;;

let default_executable_name t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "default_executable_name" in
  let open Vars.Ocamlc_config_getters in
  get vars "default_executable_name"
;;

let systhread_supported t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "systhread_supported" in
  let open Vars.Ocamlc_config_getters in
  get_bool vars "systhread_supported"
;;

let host t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "host" in
  let open Vars.Ocamlc_config_getters in
  get vars "host"
;;

let target t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "target" in
  let open Vars.Ocamlc_config_getters in
  get vars "target"
;;

let profiling t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "profiling" in
  let open Vars.Ocamlc_config_getters in
  get_bool vars "profiling"
;;

let flambda t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "flambda" in
  let open Vars.Ocamlc_config_getters in
  get_bool vars "flambda"
;;

let spacetime t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "spacetime" in
  let open Vars.Ocamlc_config_getters in
  get_bool vars "spacetime"
;;

let safe_string t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "safe_string" in
  let open Vars.Ocamlc_config_getters in
  get_bool vars "safe_string"
;;

let exec_magic_number t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "exec_magic_number" in
  let open Vars.Ocamlc_config_getters in
  get vars "exec_magic_number"
;;

let cmi_magic_number t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "cmi_magic_number" in
  let open Vars.Ocamlc_config_getters in
  get vars "cmi_magic_number"
;;

let cmo_magic_number t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "cmo_magic_number" in
  let open Vars.Ocamlc_config_getters in
  get vars "cmo_magic_number"
;;

let cma_magic_number t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "cma_magic_number" in
  let open Vars.Ocamlc_config_getters in
  get vars "cma_magic_number"
;;

let cmx_magic_number t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "cmx_magic_number" in
  let open Vars.Ocamlc_config_getters in
  get vars "cmx_magic_number"
;;

let cmxa_magic_number t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "cmxa_magic_number" in
  let open Vars.Ocamlc_config_getters in
  get vars "cmxa_magic_number"
;;

let ast_impl_magic_number t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ast_impl_magic_number" in
  let open Vars.Ocamlc_config_getters in
  get vars "ast_impl_magic_number"
;;

let ast_intf_magic_number t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "ast_intf_magic_number" in
  let open Vars.Ocamlc_config_getters in
  get vars "ast_intf_magic_number"
;;

let cmxs_magic_number t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "cmxs_magic_number" in
  let open Vars.Ocamlc_config_getters in
  get vars "cmxs_magic_number"
;;

let cmt_magic_number t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "cmt_magic_number" in
  let open Vars.Ocamlc_config_getters in
  get vars "cmt_magic_number"
;;

let windows_unicode t =
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "windows_unicode" in
  let open Vars.Ocamlc_config_getters in
  get_bool vars "windows_unicode"
;;

let natdynlink_supported t =
  (* SIMPLIFIED: Still uses filesystem check but with on-demand standard_library *)
  let standard_lib = standard_library t in
  let version_tuple = version t in
  let lib = "dynlink.cmxa" in
  let lib = if version_tuple >= (5, 0, 0) then Filename.concat "dynlink" lib else lib in
  Sys.file_exists (Filename.concat standard_lib lib)
;;

let supports_shared_libraries t =
  (* SIMPLIFIED: Just return false since we removed Makefile.config loading *)
  let vars = run_ocamlc_config_and_parse t.ocamlc_path "supports_shared_libraries" in
  let open Vars.Ocamlc_config_getters in
  get_bool vars "SUPPORTS_SHARED_LIBRARIES" ~default:false
;;

let is_dev_version t =
  let version_str = version_string t in
  Scanf.sscanf version_str "%u.%u.%u+dev" (fun _ _ _ -> ()) |> Result.is_ok
;;

let to_dyn t =
  let open Dyn in
  Record [ "ocamlc_path", String t.ocamlc_path ]
;;

let to_list _t =
  (* REMOVED: Full implementation that populated all fields *)
  []
;;

let by_name t name =
  (* SIMPLIFIED: Just does a single field lookup *)
  let vars = run_ocamlc_config_and_parse t.ocamlc_path ("by_name:" ^ name) in
  let open Vars.Ocamlc_config_getters in
  match get_opt vars name with
  | Some value -> Some (Value.String value)
  | None -> None
;;

(*
   New Create instrumented config function
*)

let create_instrumented ~ocamlc_path = { ocamlc_path }

(* simplified make vars function *)

let make _vars = Ok (create_instrumented ~ocamlc_path:"ocamlc")
