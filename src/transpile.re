/**
 * TODO: document the structure/assumptions
 */
open Semantics.Core;

exception InvalidVar string;

exception InvalidSerialization string;

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

let unwrap_expr expr => Parsetree.(expr.pexp_desc);

let get_loc_string loc => {
  let sloc = loc.Location.loc_start;
  Lexing.(Printf.sprintf "%s:%d:%d" sloc.pos_fname sloc.pos_lnum sloc.pos_bol)
};

let unwrap_var v =>
  Parsetree.(
    switch v.ppat_desc {
    | Ppat_var v_ => Location.(v_.txt)
    | _ =>
      raise (
        InvalidSerialization (
          "Invalid pattern where variable name was expected at " ^ get_loc_string v.ppat_loc
        )
      )
    }
  );

let unwrap_nat_literal expr =>
  Parsetree.(
    switch (unwrap_expr expr) {
    | Pexp_constant (Pconst_integer s None) => int_of_string s
    /* TODO to be robust, we really need int_of_string_opt . It's only avail in 4.05+. We should
          see if it's possible to upgrade reason s.t. it'll tolerate 4.05. But upgrading reason means
          changing some Reason_toolchain calls
       switch (int_of_string_opt s) {
       | None => raise_ "Non-integer string inside Pconst_integer node: "
       | Some n => n >= 0 ? n : raise_ "Hazelnut numbers must be non-negative: "
       }
       */
    | _ =>
      raise (
        InvalidSerialization (
          "Expected a simple non-negative integer literal at " ^ get_loc_string expr.pexp_loc
        )
      )
    }
  );

let unwrap_hz_lib l =>
  switch l.Location.txt {
  | Longident.Ldot (Longident.Lident "HazelPrelude") id => id
  | _ =>
    raise (
      InvalidSerialization (
        "Expected a HazelPrelude value but got something else at " ^ get_loc_string l.Location.loc
      )
    )
  };

let string_of_side side =>
  switch side {
  | HExp.L => "L"
  | HExp.R => "R"
  };

let rec hzt_to_mlt tau =>
  Ast_helper.(
    switch tau {
    | HTyp.Num => Typ.constr (mk_hz_lib "num") []
    | HTyp.Arrow tau1 tau2 => Typ.arrow Asttypes.Nolabel (hzt_to_mlt tau1) (hzt_to_mlt tau2)
    | HTyp.Sum tau1 tau2 => Typ.constr (mk_hz_lib "sum") [hzt_to_mlt tau1, hzt_to_mlt tau2]
    | HTyp.Hole => Typ.constr (mk_hz_lib "hole") []
    }
  );

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

let rec mlt_to_hzt t => {
  open Parsetree;
  let raise_ s => raise (InvalidSerialization (s ^ get_loc_string t.ptyp_loc));
  switch t.ptyp_desc {
  | Ptyp_arrow Asttypes.Nolabel t1 t2 => HTyp.Arrow (mlt_to_hzt t1) (mlt_to_hzt t2)
  | Ptyp_arrow _ _ _ => raise_ "Hazelnut arrow types can't be labeled: "
  | Ptyp_constr id args =>
    let t_ = unwrap_hz_lib id;
    switch t_ {
    | "num" when List.length args == 0 => HTyp.Num
    | "sum" when List.length args == 2 =>
      HTyp.Sum (mlt_to_hzt (List.hd args)) (mlt_to_hzt (List.nth args 1))
    | "hole" when List.length args == 0 => HTyp.Hole
    | _ => raise_ ("Invalid type HazelPrelude." ^ t_ ^ " at ")
    }
  | _ => raise_ "Invalid type at "
  }
};

let rec ml_to_hz_unsafe prstree_expr => {
  open Parsetree;
  let raise_ s => raise (InvalidSerialization (s ^ get_loc_string prstree_expr.pexp_loc));
  switch (unwrap_expr prstree_expr) {
  | Pexp_ident {Location.txt: Longident.Lident v, Location.loc: _} => HExp.Var v
  | Pexp_ident _ => raise_ "Hazelnut identifiers cannot be qualified: "
  | Pexp_constant _ => HExp.NumLit (unwrap_nat_literal prstree_expr)
  | Pexp_let Asttypes.Recursive _ _ => raise_ "Invalid recursive 'let' at "
  | Pexp_let _ [{pvb_pat: v, pvb_expr: e, pvb_attributes: _, pvb_loc: _}] sub_expr =>
    HExp.Let (unwrap_var v) (ml_to_hz_unsafe e) (ml_to_hz_unsafe sub_expr)
  | Pexp_let _ _ _ => raise_ "Invalid 'let' with multiple bindings at "
  | Pexp_fun Asttypes.Nolabel None v sub_expr => HExp.Lam (unwrap_var v) (ml_to_hz_unsafe sub_expr)
  | Pexp_fun _ _ _ _ => raise_ "Invalid 'fun' expression at "
  | Pexp_apply operator operands =>
    switch (unwrap_expr operator) {
    | Pexp_ident {Location.txt: Longident.Lident "+", Location.loc: _} =>
      switch operands {
      | [(Asttypes.Nolabel, e1), (Asttypes.Nolabel, e2)] =>
        HExp.Plus (ml_to_hz_unsafe e1) (ml_to_hz_unsafe e2)
      | _ => raise_ "'+' operator must have exactly two unlabeled operands: "
      }
    | _ =>
      switch operands {
      | [(Asttypes.Nolabel, operand)] =>
        HExp.Ap (ml_to_hz_unsafe operator) (ml_to_hz_unsafe operand)
      | _ => raise_ "A Hazelnut function must have exactly one unlabeled parameter: "
      }
    }
  | Pexp_match expr [lcase, rcase] =>
    let extract_case side => (
      fun
      | {
          pc_lhs: {ppat_desc: Ppat_construct s (Some v), ppat_loc: _, ppat_attributes: _},
          pc_guard: None,
          pc_rhs: e
        }
          when unwrap_hz_lib s == side => (
          unwrap_var v,
          ml_to_hz_unsafe e
        )
      | _ => raise_ ("Invalid " ^ side ^ " case at ")
    );
    HExp.Case (ml_to_hz_unsafe expr) (extract_case "L" lcase) (extract_case "R" rcase)
  | Pexp_match _ _ => raise_ "Invalid 'match' that doesn't correspond to Hazelnut `Case` at "
  | Pexp_construct hz_lib_loc (Some sub_expr) =>
    let constr_name = unwrap_hz_lib hz_lib_loc;
    switch constr_name {
    | "L" => HExp.Inj HExp.L (ml_to_hz_unsafe sub_expr)
    | "R" => HExp.Inj HExp.R (ml_to_hz_unsafe sub_expr)
    | "EHole" => HExp.EmptyHole (unwrap_nat_literal sub_expr)
    | "NEHole" =>
      switch (unwrap_expr sub_expr) {
      | Pexp_tuple [u, e] => HExp.NonEmptyHole (unwrap_nat_literal u) (ml_to_hz_unsafe e)
      | _ => raise_ "Invalid non-empty hole at "
      }
    | _ => raise_ ("Invalid constructor HazelPrelude." ^ constr_name ^ " at ")
    }
  | Pexp_construct _ _ => raise_ "Invalid nullary constructor at "
  | Pexp_constraint e t => HExp.Asc (ml_to_hz_unsafe e) (mlt_to_hzt t)
  | _ => raise_ "Invalid expression type at "
  }
};

let ml_to_hz prstree_expr => {
  let res = ml_to_hz_unsafe prstree_expr;
  switch (HExp.hsyn () Ctx.empty res) {
  | None => raise (InvalidSerialization "Serialization fails to type check")
  | _ => ()
  };
  res
};

let structure_to_expr =
  fun
  | [{Parsetree.pstr_desc: Parsetree.Pstr_eval e _, Parsetree.pstr_loc: _}] => e
  | _ =>
    raise (
      InvalidSerialization "Serialized hexp must have a structure [{pstr_desc: Pstr_eval, pstr_loc: _}]"
    );

let parse_ lexbuf =>
  ml_to_hz (
    structure_to_expr (fst (Reason_toolchain.JS.canonical_implementation_with_comments lexbuf))
  );

let parse i_channel => parse_ (Lexing.from_channel i_channel);

let hz_from_file filename => {
  let i_channel = open_in filename;
  let res = parse i_channel;
  close_in i_channel;
  res
};

let hz_from_string s => parse_ (Lexing.from_string s);
