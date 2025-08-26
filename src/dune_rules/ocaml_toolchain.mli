(** Toolchain information derived from an OCaml installation *)

open Import

(* type t =
  { bin_dir : Action.Prog.t
  ; ocaml : Action.Prog.t
  ; ocamlc : Action.Prog.t
  ; ocamlopt : Action.Prog.t
  ; ocamldep : Action.Prog.t
  ; ocamlmklib : Action.Prog.t
  ; ocamlobjinfo : Action.Prog.t
  ; ocaml_config : Ocaml_config.t Lazy.t
  ; ocaml_config_vars : Ocaml_config.Vars.t Lazy.t
  ; version : Ocaml.Version.t Lazy.t
  ; builtins : Meta.Simplified.t Package.Name.Map.t Memo.Lazy.t
  ; lib_config : Lib_config.t Lazy.t
  } *)
type t

(** Getter functions to access lazy fields *)
val ocaml_config : t -> Ocaml_config.t
val ocaml_config_vars : t -> Ocaml_config.Vars.t
val version : t -> Ocaml.Version.t
val lib_config : t -> Lib_config.t

val of_env_with_findlib
  :  Context_name.t
  -> Env.t
  -> Findlib_config.t option
  -> which:(Filename.t -> Path.t option Memo.t)
  -> t Memo.t

val make
  :  Context_name.t
  -> which:(string -> Path.t option Memo.t)
  -> env:Env.t
  -> get_ocaml_tool:(dir:Path.t -> string -> Path.t option Memo.t)
  -> t Memo.t

val of_binaries : path:Path.t list -> Context_name.t -> Env.t -> Path.Set.t -> t Memo.t

(** Return the compiler needed for this compilation mode *)
val compiler : t -> Ocaml.Mode.t -> Action.Prog.t

(** The best compilation mode for this context *)
val best_mode : t -> Mode.t

val check_fdo_support : t -> Context_name.t -> unit
val register_response_file_support : t -> unit