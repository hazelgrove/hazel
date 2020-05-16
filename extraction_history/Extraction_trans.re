open Extraction_declear;
open Extraction_tool;
open Extraction_decons;

// I think this file is useless now

//translate to string
let rec pass_trans = (~type1: pass_t): option(string) =>
  switch (type1) {
  | Bool => Some("bool")
  | Number => Some("int")
  | Unit => Some("()")
  | List(a) =>
    option_string_concat(~strs=[pass_trans(~type1=a), Some(" list")])
  | ARROW(a, b) =>
    option_string_concat(
      ~strs=[pass_trans(~type1=a), Some("->"), pass_trans(~type1=b)],
    )
  | SUM(a, b) =>
    option_string_concat(
      ~strs=[pass_trans(~type1=a), Some("*"), pass_trans(~type1=b)],
    )
  | PROD(a, b) =>
    option_string_concat(
      ~strs=[pass_trans(~type1=a), Some("|"), pass_trans(~type1=b)],
    )
  | EMPTY => None
  | UNK => Some("'a")
  | _ => None
  };

// translate a variable to the annotated string
// using option case of var to make future cases easy
let add_var_annotation =
    (~var: option(string), ~set: variable_set_t): option(string) =>
  switch (var) {
  | None => None
  | Some(s) =>
    option_string_concat_ignoreNone(
      ~strs=[
        var,
        option_string_concat(
          ~strs=[
            Some(":"),
            pass_trans(~type1=find_variable_set(~var=s, ~set)),
          ],
        ),
      ],
    )
  };

let var_annotate = (~var: string, ~vs: variable_set_t): extract_t =>
  extract_t_concat(
    ~le=[
      (Some("("), UNK),
      (
        add_var_annotation(~var=Some(var), ~set=vs),
        find_variable_set(~var, ~set=vs),
      ),
      (Some(")"), UNK),
    ],
  );

//================================
//  UHPat
//================================

let rec trans_uhpat_pass = (~t: UHPat.t, ~set: variable_set_t): pass_t =>
  switch (uhpat_operand(~t)) {
  | EmptyHole(_) => HOLE
  | Wild(_) => UNK
  | Var(_, _, s) => find_variable_set(~var=s, ~set)
  | NumLit(_) => Number
  | BoolLit(_) => Bool
  | ListNil(_) => List(UNK)
  | Parenthesized(a) => trans_uhpat_pass(~t=a, ~set)
  | Inj(_, _, a) => trans_uhpat_pass(~t=a, ~set)
  };

//===============================
//  UHTyp
//===============================

let rec trans_uhtyp_pass = (~t: UHTyp.t): pass_t =>
  switch (uhtyp_operand(~t)) {
  | Hole => HOLE
  | Unit => UNK
  | Num => Number
  | Bool => Bool
  | Parenthesized(a) => trans_uhtyp_pass(~t=a)
  | List(a) => List(trans_uhtyp_pass(~t=a))
  };
