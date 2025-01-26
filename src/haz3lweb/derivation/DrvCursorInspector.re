open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Haz3lcore;

let errc = "error";
let okc = "ok";
let div_err = div(~attrs=[clss(["status", errc])]);
let div_ok = div(~attrs=[clss(["status", okc])]);

let drv_view = (~globals, status: DrvInfo.t) => {
  let view_type =
    CodeViewable.view_typ(
      ~globals,
      ~settings={
        inline: true,
        fold_case_clauses: false,
        fold_fn_bodies: false,
        hide_fixpoints: false,
        fold_cast_types: false,
        show_filters: false,
      },
      ~info_map=Id.Map.empty,
    );
  let view_type = (typ: Typ.t) =>
    switch (typ.term) {
    | DrvTyp(t) => text(DrvSort.show(t))
    | _ => view_type(typ)
    };
  switch (DrvInfo.error_of(status)) {
  | None => div_ok([text("Fillable by any derivation element")])
  | Some(err) =>
    switch (err) {
    | Exp(BadToken(token))
    | Pat(BadToken(token))
    | Typ(BadToken(token))
    | TPat(BadToken(token)) =>
      div_err([text(Printf.sprintf("\"%s\" isn't a valid token", token))])
    | Exp(MultiHole)
    | Pat(MultiHole)
    | Typ(MultiHole)
    | TPat(MultiHole) => div_err([text("Expecting operator or delimiter")])
    | Exp(NoJoin(expect, actuals)) =>
      div_err([
        text(
          "Expected "
          ++ (DrvInfo.repr_ana_exp(expect) |> String.concat(", "))
          ++ ", got "
          ++ DrvInfo.repr_list_ana_exp(actuals),
        ),
      ])
    | Pat(NoJoin(expect, _)) =>
      let expect =
        switch (expect) {
        | Var => "A variable pattern"
        | Cast_Var => "A variable pattern with optional type annotation"
        | Pair_Or_Case_Var => "A pair or a variable pattern with optional type annotation"
        | Ap_InjL => "A Application of Left Injection pattern"
        | Ap_InjR => "A Application of Right Injection pattern"
        | InjL => "A Left Injection pattern"
        | InjR => "A Right Injection pattern"
        };
      div_err([text("Expected " ++ expect)]);
    | Exp(FreeVar)
    | Typ(FreeVar) => div_err([text("Unbound variable")])
    | Exp(NotVar)
    | Typ(NotVar) => div_err([text("Expected a variable")])
    | Exp(VarNoJoin(expect, actual)) =>
      div_err([
        text(
          "Expected a variable of type "
          ++ (DrvInfo.repr_ana_exp(expect) |> String.concat(", "))
          ++ ", got ",
        ),
        view_type(actual),
      ])
    | Typ(VarNoJoin(actual)) =>
      div_err([
        text("Expected a variable of type ALFA Typ, got "),
        view_type(actual),
      ])
    | Exp(TupleNotStandard) =>
      div_err([text("Expected a standard tuple expression")])
    | Exp(CaseNotStandard) =>
      div_err([text("Expected a standard case expression")])
    }
  };
};
