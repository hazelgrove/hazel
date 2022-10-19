/** Logging utilities.

    Preferences for this module can be set in the {!Params} module. */;

let info = s =>
  if (Params.log_info^) {
    prerr_endline @@ "[INFO] " ++ s;
  } else {
    ();
  };

let warn = s =>
  if (Params.log_warn^) {
    prerr_endline @@ "[WARN] " ++ s;
  } else {
    ();
  };
