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

  val has_native : t -> bool
  val ext_lib : t -> Filename.Extension.t 
  val ext_obj : t -> Filename.Extension.t 
  val os_type : t -> Ocaml_config.Os_type.t 
  val architecture :t -> string
  val system : t -> string
  val model : t  ->  string 
  val natdynlink_supported : t -> Dynlink_supported.By_the_os.t
  val ext_dll : t -> string
  val stdlib_dir : t -> Path.t
  val ccomp_type : t -> Ocaml_config.Ccomp_type.t
  val ocaml_version_string : t ->  string
  val ocaml_version : t-> Ocaml.Version.t
 

val allowed_in_enabled_if : (string * Dune_lang.Syntax.Version.t) list
val get_for_enabled_if : t -> Pform.t -> string
val linker_can_create_empty_archives : t -> bool
val hash : t -> int
val equal : t -> t -> bool
val to_dyn : t -> Dyn.t

(** [["-g"]] if [!Clflags.g] and [[]] otherwise *)
val cc_g : t -> string list

val create : Ocaml_config.t -> ocamlopt:(_, _) result -> t
