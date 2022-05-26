open Sexplib.Std;
open Sexplib;

/* Similar to the CursorPath, but TermPaths are paths relative to the tree structure of a program */

/* This works for term paths coming from CodeSummary */
[@deriving sexp]
type t = (list(ChildIndex.t), CursorInfo.cursor_term);
let cons_all =
    (step: int, steps: list(CursorPath.steps)): list(CursorPath.steps) => {
  List.map(steps => [step, ...steps], steps);
};
let rec mk_cursor_path_steps =
        (term_path: t, steps_to_term: CursorPath.steps)
        : list(CursorPath.steps) => {
  let (steps, cursor_term) = term_path;
  print_endline(
    "Term Path: " ++ Sexp.to_string(CursorPath.sexp_of_steps(steps)),
  );
  print_endline(
    "Steps to term: "
    ++ Sexp.to_string(CursorPath.sexp_of_steps(steps_to_term)),
  );
  print_endline("In mk_cursor_path_steps");
  let cursor_paths =
    switch (cursor_term) {
    | ExpOperand(_, operand) =>
      mk_cursor_path_steps_expoperand(operand, steps)
    | PatOperand(_, operand) => [
        mk_cursor_path_steps_patoperand(operand, steps),
      ]
    | TypOperand(_, operand) => [
        mk_cursor_path_steps_typoperand(operand, steps),
      ]
    | ExpOperator(_, operator, operator_index, opseq) =>
      mk_cursor_path_steps_expoperator(
        ~step_sibling=true,
        operator,
        operator_index,
        opseq,
        steps,
      )
    | PatOperator(_, operator, _operator_index, opseq) => [
        mk_cursor_path_steps_patoperator(
          ~step_sibling=true,
          operator,
          opseq,
          steps,
        ),
      ]
    | TypOperator(_, operator, _operator_index, opseq) => [
        mk_cursor_path_steps_typoperator(
          ~step_sibling=true,
          operator,
          opseq,
          steps,
        ),
      ]
    | Line(_, line, opt_body) =>
      mk_cursor_path_steps_expline(line, opt_body, steps)
    | Rule(_, rule, _index, _scrut) =>
      mk_cursor_path_steps_exprule(rule, steps)
    };
  let _ =
    List.map(
      path =>
        print_endline(
          "Cursor Path: " ++ Sexp.to_string(CursorPath.sexp_of_steps(path)),
        ),
      cursor_paths,
    );
  // Only collapses -1 that are not the first step and when there are not two -1 in a row (would only collapse first one in this case)
  let rec collapse = (steps: list(ChildIndex.t)): list(ChildIndex.t) => {
    switch (steps) {
    | [_, (-1), ...xs] => collapse(xs)
    | [x, ...xs] => [x, ...collapse(xs)]
    | [] => []
    };
  };
  List.map(steps => collapse(steps_to_term @ steps), cursor_paths);
}
and mk_cursor_path_steps_expoperand =
    (operand: UHExp.operand, term_path_steps: list(ChildIndex.t))
    : list(CursorPath.steps) => {
  print_endline("In mk_cursor_path_steps_expoperand");
  print_endline(
    "Steps to term: "
    ++ Sexp.to_string(CursorPath.sexp_of_steps(term_path_steps)),
  );
  switch (operand, term_path_steps) {
  | (EmptyHole(_), [])
  | (InvalidText(_), [])
  | (Var(_), [])
  | (IntLit(_), [])
  | (FloatLit(_), [])
  | (BoolLit(_), [])
  | (ListNil(_), []) => [[]]
  | (Lam(_, pat, _), [0, ...pat_path]) => [
      [0, ...mk_cursor_path_steps_patopseq(pat, pat_path)],
    ]
  | (Lam(_, _, _body), [1, ..._body_path]) => [[1]]
  | (Inj(_, _, _exp), [0, ..._]) => [[0]]
  | (Case(_, _scrut, _), [0, ..._]) => [[0]]
  | (Case(_, _, rules), [n1, n2, ..._]) =>
    let UHExp.Rule(_pat, _exp) = List.nth(rules, n1 - 1);
    if (n2 == 0) {
      [[n1, 0]];
    } else {
      [[n1, 1]];
    };
  | (Parenthesized(operand), steps) =>
    cons_all(0, mk_cursor_path_steps_expblock(operand, steps))
  | (ApPalette(_), _) => failwith("Not yet implemented - ApPalette")
  | _ => failwith("Doesn't work for this case - Exp Operand")
  };
}
and mk_cursor_path_steps_expblock =
    (block: UHExp.block, term_path_steps: list(ChildIndex.t))
    : list(CursorPath.steps) => {
  print_endline("In mk_cursor_path_steps_expblock");
  print_endline(Sexp.to_string(UHExp.sexp_of_block(block)));
  switch (block) {
  | [] => [[]]
  | [line] =>
    cons_all(0, mk_cursor_path_steps_expline(line, None, term_path_steps))
  | [LetLine(pat, def), ...body] =>
    cons_all(
      0,
      mk_cursor_path_steps_expline(
        LetLine(pat, def),
        Some((body, 1)),
        term_path_steps,
      ),
    )
  | lines =>
    /* TODO: Will this work always work or will the total block sometimes override the last line? */
    switch (term_path_steps) {
    | [n] => [[n]]
    | [] => List.mapi((index, _line) => [index], lines)
    | _ => failwith("Doesn't work for this case - Exp Block")
    }
  };
}
and mk_cursor_path_steps_expline =
    (
      line: UHExp.line,
      opt_body: option((UHExp.t, int)),
      term_path_steps: list(ChildIndex.t),
    )
    : list(CursorPath.steps) => {
  print_endline("In mk_cursor_path_steps_expline");
  switch (line, term_path_steps) {
  | (EmptyLine, [])
  | (CommentLine(_), []) => [[]]
  | (LetLine(pat, _def), [0, ...rest]) => [
      [0, ...mk_cursor_path_steps_patopseq(pat, rest)],
    ]
  | (LetLine(_pat, _def), [1]) => [[1]]
  | (LetLine(_pat, def), [1, ...rest]) =>
    cons_all(1, mk_cursor_path_steps_expblock(def, rest))
  | (LetLine(_pat, _def), [2]) =>
    switch (opt_body) {
    | Some((body, start_index)) =>
      List.mapi((index, _line) => [(-1), start_index + index], body)
    | None => failwith("Let line should have a body")
    }
  | (ExpLine(OpSeq(skel, seq) as opseq), _) =>
    switch (skel) {
    | Placeholder(n) =>
      let pn = Seq.nth_operand(n, seq);
      cons_all(n, mk_cursor_path_steps_expoperand(pn, term_path_steps));
    | BinOp(_, _, operator, _, _) =>
      mk_cursor_path_steps_expoperator(
        ~step_sibling=false,
        operator,
        -1,
        opseq,
        term_path_steps,
      )
    }
  | _ => failwith("Doesn't work for this case - Exp Line")
  };
}
and mk_cursor_path_steps_expopseq =
    (
      OpSeq(skel, seq) as opseq: UHExp.opseq,
      term_path_steps: list(ChildIndex.t),
    )
    : list(CursorPath.steps) => {
  print_endline("In mk_cursor_path_steps_expopseq");
  switch (skel) {
  | Placeholder(n) =>
    // Renumber the skeleton for indexing (original is needed for path construction,
    // reindexed is needed for getting nth operand)
    let OpSeq(index_skel, _) = UHExp.mk_OpSeq(seq);
    let index = Skel.get_root_num(index_skel);
    let pn = Seq.nth_operand(index, seq);
    cons_all(n, mk_cursor_path_steps_expoperand(pn, term_path_steps));
  | BinOp(_, _, operator, _, _) =>
    mk_cursor_path_steps_expoperator(
      ~step_sibling=false,
      operator,
      -1,
      opseq,
      term_path_steps,
    )
  };
}
and mk_cursor_path_steps_exprule =
    (Rule(pat, _clause): UHExp.rule, term_path_steps: list(ChildIndex.t))
    : list(CursorPath.steps) => {
  switch (term_path_steps) {
  | [(-1), 0] => [[(-1), 0]] // scrutinee path
  | [0, ...rest] => [[0, ...mk_cursor_path_steps_patopseq(pat, rest)]]
  | [1, ...rest] => [[1, ...rest]]
  | _ => failwith("Doesn't work for this case - Exp Rule")
  };
}
and mk_cursor_path_steps_expoperator =
    (
      ~step_sibling: bool,
      operator: UHExp.operator,
      _operator_index: int,
      OpSeq(skel, seq): UHExp.opseq,
      steps: list(ChildIndex.t),
    )
    : list(CursorPath.steps) => {
  print_endline("In mk_cursor_path_steps_expoperator");
  print_endline(
    "Steps to term: " ++ Sexp.to_string(CursorPath.sexp_of_steps(steps)),
  );
  switch (operator, steps) {
  | (Comma, [n, ..._]) =>
    let tuple_element = List.nth(UHExp.get_tuple_elements(skel), n);
    let element_step = Skel.get_root_num(tuple_element);
    step_sibling ? [[(-1), element_step]] : [[element_step]];
  | (_binop, [n]) =>
    print_endline(
      "Regular Skel: " ++ Sexp.to_string(UHExp.sexp_of_skel(skel)),
    );
    let (left, right) =
      switch (skel) {
      | Placeholder(_) => failwith("Can I reach here?")
      | BinOp(_, _, _, skel1, skel2) => (
          Skel.get_root_num(skel1),
          Skel.get_root_num(skel2),
        )
      };
    let child_step = n == 0 ? left : right;
    step_sibling ? [[(-1), child_step]] : [[child_step]];
  | (_binop, [n, ...rest]) =>
    /* TODO: Hannah - similar changes should probably be made for the other operators (pat & typ)*/
    print_endline(
      "Regular Skel: " ++ Sexp.to_string(UHExp.sexp_of_skel(skel)),
    );
    let (left_opseq, right_opseq) =
      switch (skel) {
      | Placeholder(_) => failwith("Can I reach here?")
      | BinOp(index, _, _, skel1, skel2) =>
        let (OpSeq(_, seq1), OpSeq(_, seq2)) =
          OpSeq.get_sub_parts_binop(index, UHExp.mk_OpSeq, seq);
        // Indices of the original skel need to be preserved
        let opseq1: UHExp.opseq = OpSeq(skel1, seq1);
        let opseq2: UHExp.opseq = OpSeq(skel2, seq2);
        (opseq1, opseq2);
      };
    let child_steps =
      n == 0
        ? mk_cursor_path_steps_expopseq(left_opseq, rest)
        : mk_cursor_path_steps_expopseq(right_opseq, rest);
    step_sibling ? cons_all(-1, child_steps) : child_steps;
  | _ => failwith("Doesn't work for this case - Exp Operator")
  };
}
and mk_cursor_path_steps_patoperand =
    (operand: UHPat.operand, term_path_steps: list(ChildIndex.t))
    : CursorPath.steps => {
  print_endline("In mk_cursor_path_steps_patoperand");
  print_endline(Sexp.to_string(UHPat.sexp_of_operand(operand)));
  print_endline(Sexp.to_string(CursorPath.sexp_of_steps(term_path_steps)));
  switch (operand, term_path_steps) {
  | (EmptyHole(_), [])
  | (Wild(_), [])
  | (Var(_), [])
  | (InvalidText(_), [])
  | (IntLit(_), [])
  | (FloatLit(_), [])
  | (BoolLit(_), [])
  | (ListNil(_), [])
  | (Inj(_), []) => []
  | (Inj(_), [0]) => [0]
  /* TODO: Make type annotations work */
  | (TypeAnn(_, operand, _typ), []) =>
    mk_cursor_path_steps_patoperand(operand, []) /* TODO: Do we want the whole type annotation highlighted? */
  | (TypeAnn(_, operand, _typ), [0, ...rest]) => [
      0,
      ...mk_cursor_path_steps_patoperand(operand, rest),
    ]
  | (TypeAnn(_, _operand, _typ), [1]) => [1] /* TODO: Get the type annotations working */
  | (Parenthesized(pat), steps) => [
      0,
      ...mk_cursor_path_steps_patopseq(pat, steps),
    ]
  | _ => failwith("Doesn't work for this case - Pat Operand")
  };
}
and mk_cursor_path_steps_patoperator =
    (
      ~step_sibling: bool,
      operator: UHPat.operator,
      OpSeq(skel, _seq): UHPat.opseq,
      steps: list(ChildIndex.t),
    )
    : CursorPath.steps => {
  print_endline("In mk_cursor_path_steps_patoperator");
  print_endline(Sexp.to_string(UHPat.sexp_of_skel(skel)));
  switch (operator, steps) {
  | (Comma, []) => []
  | (Space, []) => []
  | (_binop, [n, ..._]) =>
    let (left, right) =
      switch (skel) {
      | Placeholder(_) => failwith("Can I reach here?")
      | BinOp(_, _, _, skel1, skel2) => (
          Skel.get_root_num(skel1),
          Skel.get_root_num(skel2),
        )
      };
    let child_step = n == 0 ? left : right;
    step_sibling ? [(-1), child_step] : [child_step];
  | _ => failwith("Doesn't work for this case - Pat Operator")
  };
}
and mk_cursor_path_steps_patopseq =
    (
      OpSeq(skel, seq) as opseq: UHPat.opseq,
      term_path_steps: list(ChildIndex.t),
    )
    : CursorPath.steps => {
  print_endline("In mk_cursor_path_steps_patopseq");
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    [n, ...mk_cursor_path_steps_patoperand(pn, term_path_steps)];
  | BinOp(_, _, operator, _, _) =>
    mk_cursor_path_steps_patoperator(
      ~step_sibling=false,
      operator,
      opseq,
      term_path_steps,
    )
  };
}
and mk_cursor_path_steps_typoperand =
    (operand: UHTyp.operand, term_path_steps: list(ChildIndex.t))
    : CursorPath.steps => {
  print_endline("In mk_cursor_path_steps_typoperand");
  print_endline(Sexp.to_string(UHTyp.sexp_of_operand(operand)));
  switch (operand, term_path_steps) {
  | (Hole, [])
  | (Unit, [])
  | (Int, [])
  | (Float, [])
  | (Bool, []) => []
  | (List(_), [0]) => [0, 0]
  | (Parenthesized(typ), steps) => [
      0,
      ...mk_cursor_path_steps_typopseq(typ, steps),
    ]
  | _ => failwith("Doesn't work for this case - Typ Operand")
  };
}
and mk_cursor_path_steps_typoperator =
    (
      ~step_sibling: bool,
      operator: UHTyp.operator,
      OpSeq(skel, _seq): UHTyp.opseq,
      steps: list(ChildIndex.t),
    )
    : CursorPath.steps => {
  print_endline("In mk_cursor_path_steps_typoperator");
  print_endline(Sexp.to_string(UHTyp.sexp_of_skel(skel)));
  switch (operator, steps) {
  | (Prod, []) => []
  | (_binop, [n, ..._]) =>
    let (left, right) =
      switch (skel) {
      | Placeholder(_) => failwith("Can I reach here?")
      | BinOp(_, _, _, skel1, skel2) => (
          Skel.get_root_num(skel1),
          Skel.get_root_num(skel2),
        )
      };
    let child_step = n == 0 ? left : right;
    step_sibling ? [(-1), child_step] : [child_step];
  | _ => failwith("Doesn't work for this case - Typ Operator")
  };
}
and mk_cursor_path_steps_typopseq =
    (
      OpSeq(skel, seq) as opseq: UHTyp.opseq,
      term_path_steps: list(ChildIndex.t),
    )
    : CursorPath.steps => {
  print_endline("In mk_cursor_path_steps_typopseq");
  switch (skel) {
  | Placeholder(n) =>
    let pn = Seq.nth_operand(n, seq);
    [n, ...mk_cursor_path_steps_typoperand(pn, term_path_steps)];
  | BinOp(_, _, operator, _, _) =>
    mk_cursor_path_steps_typoperator(
      ~step_sibling=false,
      operator,
      opseq,
      term_path_steps,
    )
  };
};
