// TODO:
//     1. auto-detect type of argument, variable, return
//     2. detect the free variable and bound variable (in lambda)
//          use substitution like in dynamics
//     3. support multiple empty hole error returned
//     4. support ; and ;; in Ocaml
//     5. allow cast

// FIXME: use autofomatter to refine ocaml code of todo4. and indent

// The idea is to combine top-down and bottom-up. 
//    For example call lambda, the expression part will return a type,
//    and check with where call lambda, to create annotated

// now TODO: 1. return a option(string) -> (type, option string) pair, using fst and snd
//        -- need to pass down the type, such as "let x = 3 in x" the last x
//            maybe need pass variable set to do that
// 2. check type everywhere
// 3. the type pass will fit with UHTYP (type) and UHEXP (expressions)

open UHExp;
open Extraction_declear;
open Extraction_UHTyp;
open Extraction_UHPat;


//==============================
//  UHExp.re
//==============================

// using "fun list(_ : type) -> expr" as lambda expression
// nested is ok, fun x -> fun y -> ... -> expr

let uhexp_op_translater = (~op: UHExp.op): string =>
  switch (op) {
  | Space => " "
  | Plus => "+"
  | Minus => "-"
  | Times => "*"
  | LessThan => "<"
  | GreaterThan => ">"
  | Equals => "=="
  | Comma => ", "
  | Cons => "::"
  | And => "&&"
  | Or => "||"
  };

//TODO: complete the block and line part, add indent level indicater
//      We should force that Block([], outer nodes) should have indent 0
//      In each recursive call, we will proceed indent level by 1
//Logic: every block when it's called don't need to indent,
//        but the start of every line need to indent
//        lines follow a expression, which should be same level
//        and end of a lock doesn't insert \n
//        so indent after each line ends
let rec block_handler = (~block: block, ~level: int, ~variable_set : variable_set_t): extract_t =>
  switch (block) {
  | Block(lines, t) =>(
    option_string_concat(
      ~strs=[fst(lines_handler(~lines, ~level, ~variable_set)), fst(exp_handler(~t, ~level, ~variable_set))]
    ),
    pass_concat(~types=[snd(lines_handler(~lines, ~level, ~variable_set)), snd(exp_handler(~t, ~level, ~variable_set))])
  )
  }
//The t part
and exp_handler = (~t: t, ~level: int, ~variable_set:variable_set_t): extract_t =>
  //Not all items need the level
  //If it's a expression and no lines, no indent will apply
  switch (t) {
  // outer nodes
  | EmptyHole(_) => (None, EMPTY) //an empty hole always means incomplete
  | Var(a, b, c) =>
    switch (a, b) {
    | (NotInHole, NotInVarHole) => (add_var_annotation(~var=Some(c), ~set=variable_set), find_variable_set(~var=c, ~set=variable_set))
    | _ => (None, EMPTY)
    }
  | NumLit(a, b) =>
    switch (a) {
    | NotInHole => (Some(string_of_int(b)), Number)
    | _ => (None, EMPTY)
    }
  | BoolLit(a, b) =>
    switch (a) {
    | NotInHole => (Some(string_of_bool(b)), Bool)
    | _ => (None, EMPTY)
    }
  | ListNil(a) =>
    switch (a) {
    | NotInHole => Some("[]")
    | _ => None
    }
  // inner nodes
  | Lam(a, b, c, d) =>
    lam_handler(~errstatus=a, ~uhpat=b, ~uhtyp=c, ~block=d, ~level)
  | Inj(a, _b, c) => inj_handler(~errstatus=a, ~block=c, ~level)
  | Case(a, b, c, d) =>
    switch (a) {
    | NotInHole => case_handler(~block=b, ~rules=c, ~uhtyp=d, ~level)
    | _ => None
    }
  | Parenthesized(b) =>
    option_string_concat(
      ~strs=[Some("("), block_handler(~block=b, ~level), Some(")")],
    )
  | OpSeq(skel_t, opseq) =>
    switch (skel_t) {
    //since invariant of skel_t and opseq, decline skel_t
    | BinOp(NotInHole, _, _, _) => opseq_handler(~opseq, ~level)
    | _ => None
    }
  //TODO: ApPalette (Discard it as exceptions, since it isn't implemented now)
  | _ => None
  }
// Lam helper function
and lam_handler =
    (
      ~errstatus: ErrStatus.t,
      ~uhpat: UHPat.t,
      ~uhtyp: option(UHTyp.t),
      ~block: block,
      ~level: int,
    )
    : option(string) =>
  //UHTyp we receive a Some(Hole), it maybe legal to inference
  //Just use another version of uhtyp_translator
  switch (errstatus, uhpat, uhtyp) {
  //TODO:FIXME: another "?" issue, currently just give None
  | (NotInHole, pat, None) =>
    option_string_concat(
      ~strs=[
        Some("(fun "),
        uhpat_translater(~t=pat),
        Some(" -> "),
        block_handler(~block, ~level=level + 1),
        Some(")"),
      ],
    )
  //here we don't need indent because it's follow some expression like "="
  //but maybe writting it in a new line needs
  //FIXME: currently insert an () to protect codes, figure out whether can remove
  | (NotInHole, pat, Some(typ)) =>
    option_string_concat(
      ~strs=[
        Some("(fun "),
        uhpat_translater(~t=pat),
        Some(" : "),
        uhtyp_translater(~t=typ),
        Some(" -> "),
        block_handler(~block, ~level=level + 1),
        Some(")"),
      ],
    )
  //Maybe here need ()
  | _ => None
  }

and inj_handler =
    //Currently discard  ~injside: InjSide.t
    (~errstatus: ErrStatus.t, ~block: block, ~level: int): option(string) =>
  //FIXME: Another injection issue
  switch (errstatus) {
  | NotInHole => block_handler(~block, ~level=level + 1)
  | _ => None
  }

and opseq_handler = (~opseq: UHExp.opseq, ~level: int): option(string) =>
  switch (opseq) {
  | ExpOpExp(tm1, op, tm2) =>
    option_string_concat(
      ~strs=[
        exp_handler(~t=tm1, ~level=level + 1),
        Some(uhexp_op_translater(~op)),
        exp_handler(~t=tm2, ~level=level + 1),
      ],
    )
  | SeqOpExp(seq, op, tm) =>
    option_string_concat(
      ~strs=[
        opseq_handler(~opseq=seq, ~level=level + 1),
        Some(uhexp_op_translater(~op)),
        exp_handler(~t=tm, ~level=level + 1),
      ],
    )
  }
//put errstatus check into the main function
and case_handler =
    (
      ~block: block,
      ~rules: list(rule),
      ~uhtyp: option(UHTyp.t), //due to currently unuse
      ~level: int,
    )
    : option(string) =>
  // block is the item to switch, rules are patterns, uhtyp is the last given type
  // case a | 1 => true | 2 => false end : bool
  // FIXME: still can let ocaml to do type inference, so just discard the uhtyp
  //        indeed, ocaml has nowhere to assign type for match structure
  //  Deal with the "?" holes, let x:Num = case... don't need a type at the end
  //      whether if the "?" hole itself is an incomplete expression with Holes
  //FIXME: Currently using () to handle nested case, but a little bit ugly for the first one
  switch (uhtyp) {
  | None =>
    option_string_concat(
      ~strs=[
        Some("(match "),
        block_handler(~block, ~level),
        Some(" with"),
        rule_handler(~rules, ~level=level + 1),
        Some(")"),
      ],
    )
  | Some(typ) =>
    option_string_concat(
      ~strs=[
        Some("((match "),
        block_handler(~block, ~level),
        Some(" with"),
        rule_handler(~rules, ~level=level + 1),
        Some(") : "),
        uhtyp_translater(~t=typ),
        Some(")"),
      ],
    )
  // if uhtyp is not None, but translater result is None, it means there's incomplete hole, can't inference
  // FIXME: This is only a tricky method, so it isn't formally good
  }
// expected to output "\n  | expr => expr "
and rule_handler = (~rules: list(rule), ~level: int): option(string) =>
  switch (rules) {
  | [] => Some("")
  | [rule, ...rest] =>
    switch (rule) {
    | Rule(uhpat, block) =>
      option_string_concat(
        ~strs=[
          Some("\n"),
          Some(indent_space(~level)),
          Some("| "),
          uhpat_translater(~t=uhpat),
          Some(" -> "),
          block_handler(~block, ~level=level + 1),
          rule_handler(~rules=rest, ~level),
        ],
      )
    }
  }
and lines_handler = (~lines: list(UHExp.line), ~level: int, ~variable_set: variable_set_t): extract_t =>
  switch (lines) {
  | [] => Some("")
  | [line, ...rest] =>
    option_string_concat(
      ~strs=[
        line_handler(~line, ~level),
        lines_handler(~lines=rest, ~level),
      ],
    )
  }

// we expect the every line will return a "\n" to end the line
and line_handler = (~line: UHExp.line, ~level: int): option(string) =>
  switch (line) {
  | ExpLine(t) =>
    option_string_concat(
      ~strs=[
        exp_handler(~t, ~level),
        Some("\n"),
        Some(indent_space(~level)),
      ],
    )
  | EmptyLine => Some("\n")
  //let uhpat (: uhtyp) = block
  | LetLine(uhpat, uhtyp, block) =>
    switch (uhtyp) {
    | None =>
      option_string_concat(
        ~strs=[
          Some("let "),
          uhpat_translater(~t=uhpat),
          Some(" = "),
          block_handler(~block, ~level=level + 1),
          Some(" in\n"),
          Some(indent_space(~level)),
        ],
      )
    | Some(t) =>
      option_string_concat(
        ~strs=[
          Some("let "),
          uhpat_translater(~t=uhpat),
          Some(" : "),
          uhtyp_translater(~t),
          Some(" = "),
          block_handler(~block, ~level=level + 1),
          Some(" in\n"),
          Some(indent_space(~level)),
        ],
      )
    }
  };



//=============================

//used to handle None errors, or incomplete codes
let extraction_call = (~block: block): string =>
  switch (block_handler(~block, ~level=0)) {
  | None => "There could be some error in the code. Most possible is incomplete holes."
  //FIXME: Since currently we have no code-block in hazel, no need ";"
  //        and since use let...in, so no need other ";;"
  | Some(s) => s ++ ";;"
  };


