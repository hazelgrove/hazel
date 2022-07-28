/**
  "hazel/rt/maybe_indet.gr", handwritten.
 */
open Grain;
open Grain.Ident;
open Rt_.Stub({
       let path = "maybe_indet" |> Path.v;
     });

/**
  Implementation module.
 */
let impl = impl;

module Use = (I: Make.I) => {
  include Use(I);

  let imp = imp;

  let value = e => ctor1(v("Value"), e);
  let indet = e => ctor1(v("Indet"), e);

  let value_pat = p => pctor1(v("Value"), p);
  let indet_pat = p => pctor1(v("Indet"), p);

  let match = (scrut, use_value, use_ast) => {
    open Expr;
    let value_ident = v("v");
    let ast_ident = v("e");

    /*
      match (m) {
        | Value(v) => ...
        | Indet(e) => ...
      }
     */
    EMatch(
      scrut,
      [
        RRule(
          value_pat(Pat.var(value_ident)),
          use_value(var(value_ident)),
        ),
        RRule(indet_pat(Pat.var(ast_ident)), use_ast(var(ast_ident))),
      ],
    );
  };
};
