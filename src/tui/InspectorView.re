open Haz3lcore;
open Language;

/* Cursor inspector pane (Ctrl+T): a plain-text take on the web's
   CursorInspector. Reuses haz3lcore's ErrorPrint for mark (error)
   messages and renders the non-error Message.t payload directly. */

let pp = Typ.pretty_print;

/* Non-error payload: how this term's type was determined */
let ok_common = (c: Message.ok_common): string =>
  switch (c) {
  | Syn(ty) => "synthesized " ++ pp(ty)
  | Ana(Consistent({ana, syn, meet})) =>
    Printf.sprintf(
      "expecting %s, synthesized %s, so %s",
      pp(ana),
      pp(syn),
      pp(meet),
    )
  | Ana(InternallyInconsistent({ana, nomeet})) =>
    Printf.sprintf(
      "expecting %s; internally inconsistent: %s",
      pp(ana),
      nomeet |> List.map(pp) |> String.concat(", "),
    )
  };

let message_text = (ci: Info.t): option(string) =>
  switch (ci) {
  | InfoExp({message: Exp(Default), _})
  | InfoPat({message: Pat(Default), _}) => None
  | InfoExp({message: Exp(AnaDeferralConsistent(ty)), _}) =>
    Some("deferral; expecting " ++ pp(ty))
  | InfoExp({message: Exp(Common(c)), _})
  | InfoPat({message: Pat(Common(c)), _}) => Some(ok_common(c))
  | InfoTyp({message: Some(TypOk(Type(_))), _}) => None
  | InfoTyp({message: Some(TypOk(Variant(ctr, ty))), _}) =>
    Some(Printf.sprintf("constructor %s of %s", ctr, pp(ty)))
  | InfoTyp({message: Some(TypOk(TypeAlias(name, ty))), _}) =>
    Some(Printf.sprintf("alias %s for %s", name, pp(ty)))
  | InfoTyp({
      message: Some(TypOk(WHNormalizedTo({unnormalized, whnormalized}))),
      _,
    }) =>
    Some(
      Printf.sprintf(
        "%s normalizes to %s",
        pp(unnormalized),
        pp(whnormalized),
      ),
    )
  | InfoTPat({message: Some(TPatOk(Var(name))), _}) =>
    Some("type variable " ++ name)
  | _ => None
  };

let warning_text = (w: Warning.list_item): string =>
  switch (w) {
  | Pat(UnusedVar(name)) => "variable " ++ name ++ " is never used"
  };

let truncate = (~width: int, s: string): string => {
  let s = Util.StringUtil.escape_linebreaks(s);
  Util.Unicode.Width.columns_of_string(s) <= width
    ? s
    : fst(Frame.split_text_at_col(s, max(0, width - 1))) ++ "\xe2\x80\xa6";
};

/* The pane's content lines (sans separator), most important first */
let lines =
    (~width: int, z: Zipper.t, statics: CachedStatics.t)
    : list((Style.t, string)) =>
  switch (Indicated.ci_of(z, statics.info_map)) {
  | None => [(Theme.pane_title, "(no indicated term)")]
  | Some(ci) =>
    let cls = Info.cls_of(ci) |> Cls.show;
    let term =
      switch (ErrorPrint.term_string_of(ci)) {
      | s => String.trim(s)
      | exception _ => ""
      };
    let header = (
      Style.bold(Style.default),
      truncate(~width, cls ++ (term == "" ? "" : ":  " ++ term)),
    );
    let ty_line = (ty: Typ.t) => (
      Style.default,
      truncate(~width, "type: " ++ pp(ty)),
    );
    let marks = Info.marks_of(ci);
    let warnings = Info.warnings_of(ci);
    let problems =
      switch (marks) {
      | [] => []
      | _ => [
          (
            Theme.result_err,
            truncate(~width, "! " ++ ErrorPrint.string_of_marks(ci, marks)),
          ),
        ]
      };
    let warns =
      warnings
      |> List.map(w =>
           (
             Style.fg(Theme.yellow),
             truncate(~width, "? " ++ warning_text(w)),
           )
         );
    let types =
      switch (ci) {
      | InfoExp({ty, _})
      | InfoPat({ty, _}) => [ty_line(ty)]
      | _ => []
      };
    let how =
      switch (message_text(ci)) {
      | Some(msg) when marks == [] => [
          (Style.dim(Style.default), truncate(~width, msg)),
        ]
      | _ => []
      };
    [header] @ problems @ warns @ types @ how;
  };

let max_height = 5; /* separator + up to 4 content lines */

let height = (~size: (int, int), show: bool): int => {
  let (rows, _) = size;
  show ? min(max_height, max(2, rows / 4)) : 0;
};

let rows =
    (~width: int, ~height: int, z: Zipper.t, statics: CachedStatics.t)
    : list(Frame.row) =>
  if (height <= 0) {
    [];
  } else {
    let sep_text = " inspector ";
    let dashes = n => List.init(max(n, 0), _ => "─") |> String.concat("");
    let sep = [
      (Theme.pane_title, dashes(2) ++ sep_text),
      (Theme.pane_title, dashes(width - 2 - String.length(sep_text))),
    ];
    let content =
      lines(~width, z, statics)
      |> Util.ListUtil.take(height - 1)
      |> List.map(line => [line]);
    [sep, ...content];
  };
