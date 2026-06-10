open Haz3lcore;
open Language;

/* Bottom status bar: file/dirty state, caret position, type info at the
   cursor (the TUI's minimal stand-in for the web CursorInspector), and
   error/warning counts. */

let cursor_text = (z: Zipper.t, statics: CachedStatics.t): string =>
  switch (Indicated.ci_of(z, statics.info_map)) {
  | None => ""
  | Some(ci) =>
    let cls = Info.cls_of(ci) |> Cls.show;
    switch (ci) {
    | InfoExp({ty, _})
    | InfoPat({ty, _}) => cls ++ " : " ++ Typ.pretty_print(ty)
    | _ => cls
    };
  };

let row =
    (
      ~width: int,
      ~file: option(string),
      ~dirty: bool,
      ~status_msg: option(string),
      ~caret: Util.Point.t,
      ~statics: CachedStatics.t,
      z: Zipper.t,
    )
    : Frame.row => {
  let name = Option.value(file, ~default="[scratch]") ++ (dirty ? " *" : "");
  let pos = Printf.sprintf("%d:%d", caret.row + 1, caret.col + 1);
  let middle =
    switch (status_msg) {
    | Some(msg) => msg
    | None => cursor_text(z, statics)
    };
  let errors = List.length(statics.error_ids);
  let warnings = List.length(statics.warning_ids);
  let counts =
    (errors > 0 ? Printf.sprintf(" %d!", errors) : "")
    ++ (warnings > 0 ? Printf.sprintf(" %d?", warnings) : "");
  let left = " " ++ name ++ "  " ++ pos ++ "  " ++ middle;
  let right = counts ++ " ";
  let left_cols = Util.Unicode.Width.columns_of_string(left);
  let right_cols = Util.Unicode.Width.columns_of_string(right);
  let pad = max(0, width - left_cols - right_cols);
  [
    (Theme.status_bar, left),
    (Theme.status_bar, String.make(pad, ' ')),
    (errors > 0 ? Theme.status_error : Theme.status_bar, right),
  ];
};
