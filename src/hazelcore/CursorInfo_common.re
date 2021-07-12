open CursorInfo;

type zoperand =
  | ZExp(ZExp.zoperand)
  | ZTyp(ZTyp.zoperand)
  | ZPat(ZPat.zoperand);

let cursor_term_is_editable = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | Exp(_, exp) =>
    switch (exp) {
    | EmptyHole(_)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _) => true
    | ApPalette(_, _, _, _) => failwith("ApPalette is not implemented")
    | _ => false
    }
  | Pat(_, pat) =>
    switch (pat) {
    | EmptyHole(_)
    | Wild(_)
    | Var(_, _, _)
    | IntLit(_, _)
    | FloatLit(_, _)
    | BoolLit(_, _) => true
    | _ => false
    }
  | Typ(_, _)
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _) => false
  | Line(_, line) =>
    switch (line) {
    | EmptyLine
    | CommentLine(_) => true
    | LetLine(_)
    | ExpLine(_) => false
    }
  | Rule(_, _) => false
  };
};

let string_and_index_of_cursor_term =
    (term: CursorInfo.cursor_term): (string, int) => {
  switch (term) {
  | Pat(OnText(i), Var(_, _, s))
  | Pat(OnText(i), InvalidText(_, s))
  | Pat(OnText(i), IntLit(_, s))
  | Pat(OnText(i), FloatLit(_, s))
  | Exp(OnText(i), Var(_, _, s))
  | Exp(OnText(i), InvalidText(_, s))
  | Exp(OnText(i), IntLit(_, s))
  | Exp(OnText(i), FloatLit(_, s)) => (s, i)
  | Exp(OnText(i), BoolLit(_, b))
  | Pat(OnText(i), BoolLit(_, b)) => (string_of_bool(b), i)
  | _ => ("", 0)
  };
};

let index_of_cursor_term = (term: CursorInfo.cursor_term): int => {
  switch (term) {
  | Exp(OnText(i), _) => i
  | Pat(OnText(i), _) => i
  | _ =>
    print_endline("TODO(andrew): index_of_cursor_term fallthough");
    0;
  };
};

let is_empty_hole = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | Exp(_, EmptyHole(_)) => true
  | Exp(_, _) => false
  | Pat(_, EmptyHole(_)) => true
  | Pat(_, _) => false
  | Typ(_, Hole) => true
  | Typ(_, _) => false
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _)
  | Line(_, _)
  | Rule(_, _) => false
  };
};

let is_op = (cursor_term: cursor_term): bool => {
  switch (cursor_term) {
  | ExpOp(_, _)
  | PatOp(_, _)
  | TypOp(_, _) => true
  | _ => false
  };
};

let is_empty_line = (cursor_term): bool => {
  switch (cursor_term) {
  | Line(_, EmptyLine) => true
  | _ => false
  };
};

let is_comment_line = (cursor_term): bool => {
  switch (cursor_term) {
  | Line(_, CommentLine(_)) => true
  | _ => false
  };
};

let mk =
    (
      ~uses=?,
      ~syntactic_context=CursorInfo.NoSeq,
      ~opParent=None,
      typed,
      ctx,
      cursor_term,
    )
    : CursorInfo.t => {
  typed,
  ctx,
  uses,
  cursor_term,
  syntactic_context,
  opParent,
};
let get_ctx = (ci: CursorInfo.t) => ci.ctx;

/*
 * there are cases we can't determine where to find the uses of a variable
 * immediately after we see its binding site.
 * in this case, we will return a deferrable('t) and go up the tree
 * until we could find uses and feed it to (uses_list => 't).
 */

type deferrable('t) =
  | CursorNotOnDeferredVarPat('t)
  | CursorOnDeferredVarPat(UsageAnalysis.uses_list => 't, Var.t);
