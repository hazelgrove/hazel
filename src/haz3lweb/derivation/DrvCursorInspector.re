open Virtual_dom.Vdom;
open Node;
open Util.Web;
open Haz3lcore;

let errc = "error";
let okc = "ok";
let div_err = div(~attrs=[clss(["status", errc])]);
let div_ok = div(~attrs=[clss(["status", okc])]);

let drv_view = (status: DrvInfo.t) => {
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
    | Pat(Expect(expect)) =>
      let expect =
        switch (expect) {
        | Any => "Any pattern"
        | Var => "A variable pattern"
        | Cast_Var => "A variable pattern with optional type annotation"
        | Pair_Or_Case_Var => "A pair or a variable pattern with optional type annotation"
        | Ap_InjL => "A Application of Left Injection pattern"
        | Ap_InjR => "A Application of Right Injection pattern"
        | InjL => "A Left Injection pattern"
        | InjR => "A Right Injection pattern"
        };
      div_err([text("Expected " ++ expect)]);
    | Exp(NoJoin(ty)) when ty == Arrow =>
      // TODO(zhiyao): not sufficient
      div_err([text("Function argument type inconsistent with arrow type")])
    | Exp(NoJoin(ty)) =>
      // TODO(zhiyao): not sufficient
      div_err([
        text(
          "Expect sort "
          ++ (
            ty
            |> (
              fun
              | Jdmt => "Jdmt"
              | Ctx => "Ctx"
              | Prop => "Prop"
              | Exp => "ALFA_Exp"
              | Arrow => "???"
            )
          ),
        ),
      ])
    | Exp(FreeVar) => div_err([text("Expected a variable")])
    | Typ(FreeVar) => div_err([text("Expected a type variable")])
    }
  };
};
