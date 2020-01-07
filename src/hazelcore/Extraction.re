open UHExp;

// TODO: Discard the "?" holes in the first step
// FIXME: Then carefully consider it

// TODO: Use auto-formatter in ocaml to deal with unnecessary indent and ()s

// Add indent levels and pass into the handlers
// level starts from 0, +1 means double space
let rec indent_space = (~level: int): string =>
  if (level > 0) {
    "  " ++ indent_space(~level=level - 1);
  } else {
    "";
  };

//print_endline(indent_space(2) ++ "Hello")

//==============================
//  UHTyp.re
//==============================

// translate type constructor into ocaml type
//opseq is when using bi-argument operations like plus,
//skel.t use BinOp to indicate the operation(op) and two placeholder
//  in UHTyp, it's one the type, like Num -> Num
//  It's like skel.t will always receive NotInHole, so ignore it
//opseq is the operation, ExpOpExp is to do calculation,
//  SeqOpExp is to do sequence operation, like 1+2+3
let rec uhtyp_translater = (~t: UHTyp.t): option(string) =>
  switch (t) {
  | Hole => None
  | Num => Some("int")
  | Bool => Some("bool")
  | Unit => Some("()") //written as (), actually is unit
  | List(a) =>
    switch (uhtyp_translater(~t=a)) {
    | None => None
    | Some(s) => Some(s ++ " list")
    }
  | Parenthesized(a) =>
    switch (uhtyp_translater(~t=a)) {
    | None => None
    | Some(s) => Some("(" ++ s ++ ")")
    }
  | OpSeq(skel_t, opseq) =>
    switch (skel_t) {
    // Since skeleton is consistant with opseq, decline skel_t
    | BinOp(NotInHole, _, _, _) => uhtyp_opseq_translater(~opseq)
    | _ => None
    }
  }
and uhtyp_opseq_translater = (~opseq): option(string) =>
  switch (opseq) {
  | ExpOpExp(tm1, op, tm2) =>
    switch (uhtyp_translater(~t=tm1), uhtyp_translater(~t=tm2)) {
    | (Some(a), Some(b)) =>
      Some(a ++ " " ++ uhtyp_op_translater(~op) ++ " " ++ b)
    | _ => None
    }
  | SeqOpExp(seq, op, tm) =>
    switch (uhtyp_opseq_translater(~opseq=seq), uhtyp_translater(~t=tm)) {
    | (Some(a), Some(b)) =>
      Some(a ++ " " ++ uhtyp_op_translater(~op) ++ " " ++ b)
    | _ => None
    }
  }
and uhtyp_op_translater = (~op: UHTyp.op): string =>
  switch (op) {
  | Arrow => " -> "
  | Prod => " * " //int*int in ocaml
  | Sum => " | "
  };

//FIXME: Here is a version with type inference
//  such as (\lambda x:?.x+1) 1 can return 2, but leave a ? hole as "Hole"
//ocaml can do type inference, so just change hole to 'a
// let rec uhtyp_translater = (~t : UHTyp.t) : option(string) =>
//   switch(t) {
//     | Hole => "'a"
//     | Num => Some("int")
//     | Bool => Some("bool")
//     | Unit => Some("()") //written as (), actually is unit
//     | List(a) => switch(uhtyp_translater(a)){
//       | None => None
//       | Some(s) => Some(s ++ " list")
//     }
//     | Parenthesized(a) => switch(uhtyp_translater(a)) {
//       | None => None
//       | Some(s) => Some("(" ++ s ++ ")")
//     }
//     | OpSeq(skel_t, opseq) => switch(skel_t){
//       | BinOp(NotInHole, _, _, _) => uhtyp_opseq_translater(~opseq=opseq)
//       | _ => None
//     }
//   }
//   and uhtyp_opseq_translater = (~opseq) : option(string) => switch(opseq){
//         | ExpOpExp(tm1, op, tm2) => switch((uhtyp_translater(tm1), uhtyp_translater(tm2))){
//           | (Some(a), Some(b)) => Some(a ++ " " ++ uhtyp_op_translater(op) ++ " " ++ b)
//           | _ => None
//         }
//         | SeqOpExp(seq, op, tm) => switch(uhtyp_opseq_translater(~opseq=seq), uhtyp_translater(tm)) {
//           | (Some(a), Some(b)) => Some(a ++ " " ++ uhtyp_op_translater(op) ++ " " ++ b)
//           | _ => None
//         }
//   } and uhtyp_op_translater = (~op : UHTyp.op) : string =>
//   switch(op) {
//     | Arrow => " -> "
//     | Prod => " * " //int*int in ocaml
//     | Sum => " | "
//   };

// TESTCASE

// let uhtyp_example1 : UHTyp.t= Parenthesized(OpSeq(
//     BinOp(NotInHole, Arrow, Placeholder(1), Placeholder(2)),
//     ExpOpExp(Num, Arrow, Num)));

// let uhtyp_example2 : UHTyp.t= Parenthesized(OpSeq(
//     BinOp(NotInHole, Arrow, BinOp(NotInHole, Arrow, Placeholder(0), Placeholder(1)), Placeholder(2)),
//     SeqOpExp(ExpOpExp(Unit, Arrow, Bool), Arrow, Num)));

// switch(uhtyp_translater(~t=uhtyp_example2)){
//   | None => ()
//   | Some(s) => print_endline(s)
// };

//==============================
// UHPat.re
//==============================

let rec uhpat_translater = (~t: UHPat.t): option(string) =>
  switch (t) {
  | EmptyHole(_) => None
  | Wild(a) =>
    switch (a) {
    | NotInHole => Some("_")
    | _ => None
    }
  | Var(a, b, c) =>
    switch (a, b) {
    | (NotInHole, NotInVarHole) => Some(c)
    | _ => None
    }
  | NumLit(a, b) =>
    switch (a) {
    | NotInHole => Some(string_of_int(b))
    | _ => None
    }
  | BoolLit(a, b) =>
    switch (a) {
    | NotInHole => Some(string_of_bool(b))
    | _ => None
    }
  | ListNil(a) =>
    switch (a) {
    | NotInHole => Some("[]")
    | _ => None
    }
  | Parenthesized(t) =>
    switch (uhpat_translater(~t)) {
    | None => None
    | Some(s) => Some("(" ++ s ++ ")")
    }
  //FIXME: currently we use polymorphic type ('a) for it in "Let" assignment,
  // better to reconstruct for a type
  // (though the inference is good for that)
  // in Hazel, a type Num | Bool can't have value 1,
  //   it should be type inj[L or R, Bool](1)
  // in ocaml, sum type should be directly give value by constructor
  //   and need a type declaration for sum type
  // Hence change "x : (A | B) = inj[L](val)" where val:A into
  //   "x : 'a = val" temporarily, :'a can be ignored
  //   here we ignore 'a, because imagine assignment is legal
  | Inj(a, _b, c) =>
    switch (a) {
    | NotInHole => uhpat_translater(~t=c)
    | _ => None
    }
  | OpSeq(skel_t, opseq) =>
    switch (skel_t) {
    | BinOp(NotInHole, _, _, _) => uhpat_opseq_translater(~opseq)
    | _ => None
    }
  }
and uhpat_opseq_translater = (~opseq): option(string) =>
  switch (opseq) {
  | ExpOpExp(tm1, op, tm2) =>
    switch (uhpat_translater(~t=tm1), uhpat_translater(~t=tm2)) {
    | (Some(a), Some(b)) =>
      Some(a ++ " " ++ uhpat_op_translater(~op) ++ " " ++ b)
    | _ => None
    }
  | SeqOpExp(seq, op, tm) =>
    switch (uhpat_opseq_translater(~opseq=seq), uhpat_translater(~t=tm)) {
    | (Some(a), Some(b)) =>
      Some(a ++ " " ++ uhpat_op_translater(~op) ++ " " ++ b)
    | _ => None
    }
  }
and uhpat_op_translater = (~op: UHPat.op): string =>
  switch (op) {
  | Comma => ", "
  | Space => " "
  | Cons => " :: "
  };

//TESTCASE

// let uhpat_example1 : UHPat.t = Parenthesized(OpSeq(
//     BinOp(NotInHole, Cons, BinOp(NotInHole, Cons, Placeholder(0), Placeholder(1)), Placeholder(2)),
//     SeqOpExp(ExpOpExp(NumLit(NotInHole, 1), Cons, NumLit(NotInHole, 2)), Cons, ListNil(NotInHole))));

// let uhpat_example2 : UHPat.t = Parenthesized(Inj(NotInHole, L, Parenthesized(OpSeq(
//     BinOp(NotInHole, Cons, BinOp(NotInHole, Cons, Placeholder(0), Placeholder(1)), Placeholder(2)),
//     SeqOpExp(ExpOpExp(NumLit(NotInHole, 1), Cons, NumLit(NotInHole, 2)), Cons, ListNil(NotInHole))))))

// switch(uhpat_translater(~t=uhpat_example2)){
//   | None => ()
//   | Some(s) => print_endline(s)
// };

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
let rec block_handler = (~block: block, ~level: int): option(string) =>
  switch (block) {
  | Block(_lines, t) => type_handler(~t, ~level)
  }
//The t part
and type_handler = (~t: t, ~level: int): option(string) =>
  //Not all items need the level
  //If it's a expression and no lines, no indent will apply
  switch (t) {
  // outer nodes
  | EmptyHole(_) => None //an empty hole always means incomplete
  | Var(a, b, c) =>
    switch (a, b) {
    | (NotInHole, NotInVarHole) => Some(c)
    | _ => None
    }
  | NumLit(a, b) =>
    switch (a) {
    | NotInHole => Some(string_of_int(b))
    | _ => None
    }
  | BoolLit(a, b) =>
    switch (a) {
    | NotInHole => Some(string_of_bool(b))
    | _ => None
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
  | Case(a, b, c, _d) =>
    switch (a) {
    | NotInHole => case_handler(~block=b, ~rules=c, ~level)
    | _ => None
    }
  | Parenthesized(b) =>
    switch (block_handler(~block=b, ~level)) {
    | None => None
    | Some(s) => Some("(" ++ s ++ ")")
    }
  | OpSeq(skel_t, opseq) =>
    switch (skel_t) {
    //since invariant of skel_t and opseq, decline skel_t
    | BinOp(NotInHole, _, _, _) => opseq_handler(~opseq, ~level)
    | _ => None
    }
  //TODO: ApPalette (Discard it as exceptions)
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
  | (NotInHole, pat, None) =>
    switch (
      uhpat_translater(~t=pat),
      block_handler(~block, ~level=level + 1),
    ) {
    //here we don't need indent because it's follow some expression like "="
    //but maybe writting it in a new line needs
    //FIXME: currently insert an () to protect codes, figure out whether can remove
    | (Some(s), Some(b)) => Some("(fun " ++ s ++ " -> " ++ b ++ ")")
    | _ => None
    }
  | (NotInHole, pat, Some(typ)) =>
    switch (
      uhpat_translater(~t=pat),
      uhtyp_translater(~t=typ),
      block_handler(~block, ~level=level + 1),
    ) {
    //Maybe here need ()
    | (Some(s), Some(t), Some(b)) =>
      Some("(fun " ++ s ++ ":" ++ t ++ " -> " ++ b ++ ")")
    | _ => None
    }
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
    switch (
      type_handler(~t=tm1, ~level=level + 1),
      type_handler(~t=tm2, ~level=level + 1),
    ) {
    | (Some(a), Some(b)) =>
      Some(a ++ " " ++ uhexp_op_translater(~op) ++ " " ++ b)
    | _ => None
    }
  | SeqOpExp(seq, op, tm) =>
    switch (
      opseq_handler(~opseq=seq, ~level=level + 1),
      type_handler(~t=tm, ~level=level + 1),
    ) {
    | (Some(a), Some(b)) =>
      Some(a ++ " " ++ uhexp_op_translater(~op) ++ " " ++ b)
    | _ => None
    }
  }
//put errstatus check into the main function
and case_handler =
    (
      ~block: block,
      ~rules: list(rule),
      //      ~uhtyp: option(UHTyp.t),  //due to currently unuse
      ~level: int,
    )
    : option(string) =>
  // block is the item to switch, rules are patterns, uhtyp is the last given type
  // case a | 1 => true | 2 => false end : bool
  // FIXME: still can let ocaml to do type inference, so just discard the uhtyp
  //        indeed, ocaml has nowhere to assign type for match structure
  // FIXME: Deal with the "?" holes, let x:Num = case... don't need a type at the end
  //      whether if the "?" hole itself is an incomplete expression with Holes
  switch (
    block_handler(~block, ~level),
    rule_handler(~rules, ~level=level + 1),
  ) {
  //FIXME: Currently using () to handle nested case, but a little bit ugly for the first one
  | (Some(b), Some(r)) => Some("(match " ++ b ++ " with" ++ r ++ ")")
  // match should be after some expression, so don't indent
  | _ => None
  }
// expected to output "\n  | expr => expr "
and rule_handler = (~rules: list(rule), ~level: int): option(string) =>
  switch (rules) {
  | [] => Some("")
  | [rule, ...rest] =>
    switch (rule, rule_handler(~rules=rest, ~level)) {
    | (Rule(uhpat, block), Some(result)) =>
      switch (
        uhpat_translater(~t=uhpat),
        block_handler(~block, ~level=level + 1),
      ) {
      // Don't proceed on level because all are same level
      | (Some(t), Some(expr)) =>
        Some(
          "\n"
          ++ indent_space(~level)
          ++ "| "
          ++ t
          ++ " -> "
          ++ expr
          ++ result,
        )
      | _ => None
      }
    | _ => None
    }
  };

//=============================

//used to handle None errors, or incomplete codes
let extraction_call = (~block: block): string =>
  switch (block_handler(~block, ~level=0)) {
  | None => "There could be some error in the code. Most possible is incomplete holes."
  | Some(s) => s
  };

//TESTCASE

// an example of
// let x=3 in
// x
let example_let =
  Block(
    [
      LetLine(
        UHPat.Var(NotInHole, NotInVarHole, "x"),
        None,
        Block([], NumLit(NotInHole, 3)),
      ),
    ],
    Var(NotInHole, NotInVarHole, "x"),
  );

// an example of simple 123
let example_123 = Block([], NumLit(NotInHole, 123));

let example_true = Block([], BoolLit(NotInHole, true));

let example_emptyhole = Block([], EmptyHole(45));

let example_listnil = Block([], ListNil(NotInHole));

let example_lam1 =
  Block(
    [],
    Lam(
      NotInHole,
      Var(NotInHole, NotInVarHole, "x"),
      None,
      Block([], Var(NotInHole, NotInVarHole, "x")),
    ),
  );

let example_lam2 =
  Block(
    [],
    Lam(
      NotInHole,
      Var(NotInHole, NotInVarHole, "x"),
      Some(Num),
      Block(
        [],
        Lam(
          NotInHole,
          Var(NotInHole, NotInVarHole, "y"),
          Some(Num),
          Block(
            [],
            Var(NotInHole, NotInVarHole, "xy") //need to modify
          ),
        ),
      ),
    ),
  );

let case_example1 =
  Block(
    [],
    Case(
      NotInHole,
      Block([], Var(NotInHole, NotInVarHole, "x")),
      [
        Rule(ListNil(NotInHole), Block([], NumLit(NotInHole, 0))),
        Rule(
          Var(NotInHole, NotInVarHole, "a"),
          Block([], BoolLit(NotInHole, true)),
        ),
      ],
      Some(Unit),
    ),
  );

let case_example2 =
  Block(
    [],
    Case(
      NotInHole,
      Block([], Var(NotInHole, NotInVarHole, "x")),
      [
        Rule(
          ListNil(NotInHole),
          Block(
            [],
            Lam(
              NotInHole,
              Var(NotInHole, NotInVarHole, "x"),
              Some(Num),
              Block(
                [],
                Lam(
                  NotInHole,
                  Var(NotInHole, NotInVarHole, "y"),
                  Some(Num),
                  Block(
                    [],
                    Var(NotInHole, NotInVarHole, "xy") //need to modify
                  ),
                ),
              ),
            ),
          ),
        ),
        Rule(
          Var(NotInHole, NotInVarHole, "a"),
          Block([], BoolLit(NotInHole, true)),
        ),
        Rule(
          Wild(NotInHole),
          Block(
            [],
            Case(
              NotInHole,
              Block([], Var(NotInHole, NotInVarHole, "y")),
              [
                Rule(ListNil(NotInHole), Block([], NumLit(NotInHole, 1))),
                Rule(
                  Var(NotInHole, NotInVarHole, "b"),
                  Block([], BoolLit(NotInHole, true)),
                ),
              ],
              Some(Unit),
            ),
          ),
        ),
        Rule(
          Var(NotInHole, NotInVarHole, "a"),
          Block([], BoolLit(NotInHole, true)),
        ),
      ],
      Some(Unit),
    ),
  );

let parenthesized_example1 = Block([], Parenthesized(case_example2));

print_endline(extraction_call(~block=parenthesized_example1));
