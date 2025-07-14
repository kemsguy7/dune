open Import

type t =
  { has_native : bool
  ; ext_lib : Filename.Extension.t
  ; ext_obj : Filename.Extension.t
  ; os_type : Ocaml_config.Os_type.t
  ; architecture : string
  ; system : string
  ; model : string
  ; natdynlink_supported : Dynlink_supported.By_the_os.t
  ; ext_dll : string
  ; stdlib_dir : Path.t
  ; ccomp_type : Ocaml_config.Ccomp_type.t
  ; ocaml_version_string : string
  ; ocaml_version : Ocaml.Version.t
  }

(* DEBUG INSTRUMENTATION - Track which fields are accessed *)
let debug_access field_name value =
  Printf.eprintf ":[FIELD] lib_config.%s accessed!\n%!" field_name;
  value
;;

(* Debug wrapper functions for each field *)
[@@@warning "-32"] (* I'm disabling unused variable warning for this section *)

(* Constructor function - like a class constructor in TypeScript *)
let make
      ~has_native
      ~ext_lib
      ~ext_obj
      ~os_type
      ~architecture
      ~system
      ~model
      ~natdynlink_supported
      ~ext_dll
      ~stdlib_dir
      ~ccomp_type
      ~ocaml_version_string
      ~ocaml_version
  =
  { has_native
  ; ext_lib
  ; ext_obj
  ; os_type
  ; architecture
  ; system
  ; model
  ; natdynlink_supported
  ; ext_dll
  ; stdlib_dir
  ; ccomp_type
  ; ocaml_version_string
  ; ocaml_version
  }
;;

let has_native t = debug_access "has_native" t.has_native
let ext_lib t = debug_access "ext_lib" t.ext_lib
let ext_obj t = debug_access "ext_obj" t.ext_obj
let os_type t = debug_access "os_type" t.os_type
let architecture t = debug_access "architecture" t.architecture
let system t = debug_access "system" t.system
let model t = debug_access "model" t.model
let natdynlink_supported t = debug_access "natdynlink_supported" t.natdynlink_supported
let ext_dll t = debug_access "ext_dll" t.ext_dll
let stdlib_dir t = debug_access "stdlib_dir" t.stdlib_dir
let ccomp_type t = debug_access "ccomp_type" t.ccomp_type
let ocaml_version_string t = debug_access "ocaml_version_string" t.ocaml_version_string
let ocaml_version t = debug_access "ocaml_version" t.ocaml_version

[@@@warning "+32"] (* Re-enable unused value warnings *)

let allowed_in_enabled_if =
  [ "architecture", (1, 0)
  ; "system", (1, 0)
  ; "model", (1, 0)
  ; "os_type", (1, 0)
  ; "ccomp_type", (2, 0)
  ; "ocaml_version", (2, 5)
  ]
;;

let get_for_enabled_if t (pform : Pform.t) =
  match pform with
  | Var Architecture -> debug_access "architecture" t.architecture
  | Var System -> debug_access "system" t.system
  | Var Model -> debug_access "model" t.model
  | Var Os_type -> debug_access "os_type" (Ocaml_config.Os_type.to_string t.os_type)
  | Var Ccomp_type ->
    debug_access "ccomp_type" (Ocaml_config.Ccomp_type.to_string t.ccomp_type)
  | Var Ocaml_version -> debug_access "ocaml_version_string" t.ocaml_version_string
  | _ ->
    Code_error.raise
      "Lib_config.get_for_enabled_if: var not allowed"
      [ "var", Pform.to_dyn pform ]
;;

let linker_can_create_empty_archives t =
  match debug_access "ccomp_type" t.ccomp_type with
  | Msvc -> false
  | Cc | Other _ -> true
;;

let hash = Poly.hash
let equal = Poly.equal
let to_dyn = Dyn.opaque

let cc_g t =
  match debug_access "ccomp_type" t.ccomp_type with
  | Msvc -> []
  | Cc | Other _ -> [ "-g" ]
;;

let create ocaml_config ~ocamlopt =
  Printf.eprintf
    "[FIELD] lib_config.create called - creating lib_config from ocaml_config\n%!";
  { has_native = Result.is_ok ocamlopt
  ; ext_obj = Ocaml_config.ext_obj ocaml_config
  ; ext_lib = Ocaml_config.ext_lib ocaml_config
  ; os_type = Ocaml_config.os_type ocaml_config
  ; architecture = Ocaml_config.architecture ocaml_config
  ; system = Ocaml_config.system ocaml_config
  ; model = Ocaml_config.model ocaml_config
  ; ext_dll = Ocaml_config.ext_dll ocaml_config
  ; natdynlink_supported =
      (let natdynlink_supported = Ocaml_config.natdynlink_supported ocaml_config in
       Dynlink_supported.By_the_os.of_bool natdynlink_supported)
  ; stdlib_dir = Path.of_string (Ocaml_config.standard_library ocaml_config)
  ; ccomp_type = Ocaml_config.ccomp_type ocaml_config
  ; ocaml_version_string = Ocaml_config.version_string ocaml_config
  ; ocaml_version = Ocaml.Version.of_ocaml_config ocaml_config
  }
;;
