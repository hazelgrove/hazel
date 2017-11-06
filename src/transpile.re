/**
 * TODO: document the structure/assumptions
 */
open Semantics.Core;

exception InvalidVar string;

/* Utility functions */
let is_valid_var x => {
  /* Ocaml supports regex via the str module, but Js has a different regex lib,
     so which regex we can use may depend on the compilation target, which this
     file probably shouldn't assume. For now, we'll do this the hard way */
  let is_lower c => c >= 'a' && c <= 'z';
  let is_upper c => c >= 'A' && c <= 'Z';
  let is_numeric c => c >= '0' && c <= '9';
  let is_underscore c => c == '_';
  let len = String.length x;
  let rec check_rem i =>
    i == len || {
      let c = x.[i];
      (is_lower c || is_upper c || is_numeric c || is_underscore c) && check_rem (i + 1)
    };
  len > 0 && {
    let first = x.[0];
    (is_underscore first || is_lower first) && check_rem 1
  }
};

let ensure_valid s => is_valid_var s ? s : raise (InvalidVar s);

let mk_loc v => Location.{loc: none, txt: v};

let mk_hz_lib s => mk_loc Longident.(Ldot (Lident "HazelPrelude") s);

let mk_var s => Ast_helper.Pat.var (mk_loc (ensure_valid s));

let mk_int n => Parsetree.Pconst_integer (string_of_int n) None;

let string_of_side side =>
  switch side {
  | HExp.L => "L"
  | HExp.R => "R"
  };

/* HTyp */
let rec hzt_to_mlt tau =>
  Ast_helper.(
    switch tau {
    | HTyp.Num => Typ.constr (mk_hz_lib "num") []
    | HTyp.Arrow tau1 tau2 => Typ.arrow Asttypes.Nolabel (hzt_to_mlt tau1) (hzt_to_mlt tau2)
    | HTyp.Sum tau1 tau2 => Typ.constr (mk_hz_lib "sum") [hzt_to_mlt tau1, hzt_to_mlt tau2]
    | HTyp.Hole => Typ.constr (mk_hz_lib "hole") []
    }
  );

/* HExp */
let rec hz_to_ml e =>
  Ast_helper.(
    switch e {
    | HExp.Asc e' tau => Exp.constraint_ (hz_to_ml e') (hzt_to_mlt tau)
    | HExp.Var x => Exp.ident (mk_loc (Longident.Lident (ensure_valid x)))
    | HExp.Let x e e' =>
      Exp.let_ Asttypes.Nonrecursive [Vb.mk (mk_var x) (hz_to_ml e)] (hz_to_ml e')
    | HExp.Lam x e' => Exp.fun_ Asttypes.Nolabel None (mk_var x) (hz_to_ml e')
    | HExp.Ap e1 e2 => Exp.apply (hz_to_ml e1) [(Asttypes.Nolabel, hz_to_ml e2)]
    | HExp.NumLit n => Exp.constant (mk_int n)
    | HExp.Plus e1 e2 =>
      Exp.apply
        (Exp.ident (mk_loc (Longident.Lident "+")))
        [(Asttypes.Nolabel, hz_to_ml e1), (Asttypes.Nolabel, hz_to_ml e2)]
    | HExp.Inj side e => Exp.construct (mk_hz_lib (string_of_side side)) (Some (hz_to_ml e))
    | HExp.Case e1 (x, e2) (y, e3) =>
      let mk_case side v exp =>
        Exp.case (Pat.construct (mk_hz_lib side) (Some (mk_var v))) (hz_to_ml exp);
      Exp.match_ (hz_to_ml e1) [mk_case "L" x e2, mk_case "R" y e3]
    | HExp.EmptyHole u => Exp.construct (mk_hz_lib "EHole") (Some (Exp.constant (mk_int u)))
    | HExp.NonEmptyHole u e =>
      Exp.construct (mk_hz_lib "NEHole") (Some (Exp.tuple [Exp.constant (mk_int u), hz_to_ml e]))
    }
  );

let expr_to_structure expr =>
  /* TODO Ast_helper.[Str.value Asttypes.Nonrecursive [Vb.mk (mk_var "v") expr]]; */
  [Ast_helper.Str.eval expr];

let serialize formatter hexp =>
  Reason_toolchain.JS.print_canonical_implementation_with_comments
    formatter (expr_to_structure (hz_to_ml hexp), []);

let hz_to_string hexp => {
  let buf = Buffer.create 32;
  let fmtr = Format.formatter_of_buffer buf;
  serialize fmtr hexp;
  Format.pp_print_flush fmtr ();
  Buffer.contents buf
};

let hz_to_file filename hexp => {
  let out = open_out filename;
  serialize (Format.formatter_of_out_channel out) hexp;
  close_out out
};
