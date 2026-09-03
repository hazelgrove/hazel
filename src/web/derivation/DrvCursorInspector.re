open Virtual_dom.Vdom;
open Node;
open Util.WebUtil;
open Language;

let errc = "error";
let okc = "ok";
let div_err = div(~attrs=[clss(["status", errc])]);
let div_ok = div(~attrs=[clss(["status", okc])]);

let drv_view = (~globals, status: DrvInfo.t) => {
  let view_type =
    CodeViewable.view_typ(
      ~globals,
      ~settings=
        Haz3lcore.ExpToSegment.Settings.{
          secondary: AutoFormat,
          parenthesization: Defensive,
          label_format: QuoteWhenNecessary,
          inline: true,
          fold_case_clauses: false,
          fold_fn_bodies: `NoFold,
          hide_fixpoints: false,
          show_filters: false,
          show_ascriptions: false,
          show_unknown_as_hole: false,
          hole_tiles: false,
          project_tables: false,
          project_html: false,
        },
    );
  let view_type = (typ: Typ.t) =>
    switch (typ.term) {
    | DrvQuoteTy(t) => text(DrvSort.to_string(t))
    | _ => view_type(typ)
    };
  switch (DrvInfo.error_of(status)) {
  | None => div_ok([])
  | Some(err) =>
    switch (err) {
    | BadToken(token) =>
      div_err([text(Printf.sprintf("\"%s\" isn't a valid token", token))])
    | MultiHole => div_err([text("Expecting operator or delimiter")])
    | NoJoin(expect, _) =>
      div_err([
        text("Unexpected term for sort " ++ (expect |> DrvSort.to_string)),
      ])
    | FreeVar => div_err([text("Unbound variable")])
    | VarNoJoin(expect, actual) =>
      div_err([
        text(
          "Expected a variable of sort "
          ++ (expect |> DrvSort.to_string)
          ++ ", got ",
        ),
        view_type(actual),
      ])
    }
  };
};
