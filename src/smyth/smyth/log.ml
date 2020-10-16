(** Logging utilities.

    Preferences for this module can be set in the {!Params} module. *)

let info s = if !Params.log_info then prerr_endline @@ "[INFO] " ^ s else ()

let warn s = if !Params.log_warn then prerr_endline @@ "[WARN] " ^ s else ()
