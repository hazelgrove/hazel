open Sexplib.Std;

[@deriving sexp]
type operator = Operators_Exp.t;

[@deriving sexp]
type t = block
and block = list(line)
and line =
  | EmptyLine
  | CommentLine(string)
  | LetLine(UHPat.t, option(UHTyp.t), t)
  | ExpLine(opseq)
  | StructLine(UHPat.t, unit, t)
and opseq = OpSeq.t(operand, operator)
and operand =
  | EmptyHole(MetaVar.t)
  | InvalidText(MetaVar.t, string)
  | Var(ErrStatus.t, VarErrStatus.t, Var.t)
  | IntLit(ErrStatus.t, string)
  | FloatLit(ErrStatus.t, string)
  | BoolLit(ErrStatus.t, bool)
  | ListNil(ErrStatus.t)
  | Lam(ErrStatus.t, UHPat.t, option(UHTyp.t), t)
  | Inj(ErrStatus.t, InjSide.t, t)
  | Case(CaseErrStatus.t, t, rules)
  | Parenthesized(t)
  | ApPalette(ErrStatus.t, PaletteName.t, SerializedModel.t, splice_info)
  | Label(LabelErrStatus.t, Label.t)
  | Prj(ErrStatus.t, operand, Label.t)
and rules = list(rule)
and rule =
  | Rule(UHPat.t, t)
and splice_info = SpliceInfo.t(t)
and splice_map = SpliceInfo.splice_map(t);

[@deriving sexp]
type skel = OpSeq.skel(operator);
[@deriving sexp]
type seq = OpSeq.seq(operand, operator);

type affix = Seq.affix(operand, operator);

let letline = (p: UHPat.t, ~ann: option(UHTyp.t)=?, def: t): line =>
  LetLine(p, ann, def);

let var =
    (
      ~err: ErrStatus.t=NotInHole,
      ~var_err: VarErrStatus.t=NotInVarHole,
      x: Var.t,
    )
    : operand =>
  Var(err, var_err, x);

let intlit = (~err: ErrStatus.t=NotInHole, n: string): operand =>
  IntLit(err, n);

let floatlit = (~err: ErrStatus.t=NotInHole, f: string): operand =>
  FloatLit(err, f);

let boollit = (~err: ErrStatus.t=NotInHole, b: bool): operand =>
  BoolLit(err, b);

let label = (~err: LabelErrStatus.t=NotInLabelHole, l: Label.t): operand =>
  Label(err, l);

let prj = (~err: ErrStatus.t=NotInHole, op: operand, l: Label.t): operand =>
  Prj(err, op, l);

let lam =
    (
      ~err: ErrStatus.t=NotInHole,
      p: UHPat.t,
      ~ann: option(UHTyp.t)=?,
      body: t,
    )
    : operand =>
  Lam(err, p, ann, body);

let case =
    (
      ~err: CaseErrStatus.t=StandardErrStatus(NotInHole),
      scrut: t,
      rules: rules,
    )
    : operand =>
  Case(err, scrut, rules);

let listnil = (~err: ErrStatus.t=NotInHole, ()): operand => ListNil(err);

module Line = {
  let prune_empty_hole = (line: line): line =>
    switch (line) {
    | ExpLine(OpSeq(_, S(EmptyHole(_), E))) => EmptyLine
    | CommentLine(_)
    | ExpLine(_)
    | EmptyLine
    | LetLine(_)
    | StructLine(_) => line
    };

  let get_opseq =
    fun
    | EmptyLine
    | CommentLine(_)
    | LetLine(_)
    | StructLine(_) => None
    | ExpLine(opseq) => Some(opseq)
    ;

  let force_get_opseq = line =>
    line
    |> get_opseq
    |> OptUtil.get(_ => failwith("force_get_opseq: expected ExpLine"));
};

module Lines = {
  let prune_empty_holes = (lines: list(line)): list(line) =>
    lines |> List.map(Line.prune_empty_hole);
};

module Block = {
  let wrap' = (opseq: opseq): block => [ExpLine(opseq)];
  let wrap = (operand: operand): block => wrap'(OpSeq.wrap(operand));

  let num_lines: block => int = List.length;

  let prune_empty_hole_lines = (block: block): block =>
    switch (block |> ListUtil.split_last_opt) {
    | None => block
    | Some((leading, last)) => (leading |> Lines.prune_empty_holes) @ [last]
    };

  let split_conclusion = (block: block): option((list(line), opseq)) =>
    switch (block |> ListUtil.split_last_opt) {
    | None => None
    | Some((leading, last)) =>
      switch (last |> Line.get_opseq) {
      | None => None
      | Some(opseq) => Some((leading, opseq))
      }
    };
  let force_split_conclusion = (block: block): (list(line), opseq) =>
    switch (block |> split_conclusion) {
    | None => failwith("force_split_conclusion: unconcluded block")
    | Some((leading, conclusion)) => (leading, conclusion)
    };

  let join_conclusion = (leading: list(line), conclusion: opseq): block =>
    leading @ [ExpLine(conclusion)];
};

let rec get_tuple_elements: skel => list(skel) =
  fun
  | BinOp(_, Comma, skel1, skel2) =>
    get_tuple_elements(skel1) @ get_tuple_elements(skel2)
  | skel => [skel];

let rec get_projected_skel =
        (elements: list(skel), seq: seq, l: Label.t): option(skel) => {
  switch (elements) {
  | [] => None
  | [BinOp(_, Space, Placeholder(n), skel2), ...skels] =>
    let en = seq |> Seq.nth_operand(n);
    switch (en) {
    | Label(NotInLabelHole, l') =>
      if (l' == l) {
        Some(skel2);
      } else {
        get_projected_skel(skels, seq, l);
      }
    | _ => get_projected_skel(skels, seq, l)
    };
  | [_, ...skels] => get_projected_skel(skels, seq, l)
  };
};

let rec mk_tuple = (~err: ErrStatus.t=NotInHole, elements: list(skel)): skel =>
  switch (elements) {
  | [] => failwith("mk_tuple: expected at least 1 element")
  | [skel] => skel
  | [skel, ...skels] => BinOp(err, Comma, skel, mk_tuple(skels))
  };

let desugar_struct = (strct: line): option(line) =>
  switch (strct) {
  | StructLine(p, _, [_, ..._] as def) =>
    let rec bindings_of_let_lines =
            (~acc: list(Var.t)=[], l: block): option(list(Var.t)) =>
      switch (l) {
      | [ExpLine(_)] => acc == [] ? None : Some(acc)
      | [LetLine(p, _, _), ...tl] =>
        switch (p) {
        // TODO (hejohns): check error conditions?
        | OpSeq(_, S(Var(_, NotInVarHole, name), E)) =>
          bindings_of_let_lines(~acc=acc @ [name], tl)
        | _ => None
        }
      | _ => None
      };
    switch (bindings_of_let_lines(def)) {
    | Some(bindings) =>
      let mk_struct_record = (l: list(Var.t)): line =>
        switch (l) {
        | [] => failwith("expected at least one element")
        | [hd, ..._] =>
          open Operators_Exp;
          let rec mk_tl_seq =
                  (op: operator, bindings: list(Var.t))
                  : list((operator, operand)) =>
            switch (bindings) {
            | [] => []
            | [hd, ...tl] =>
              switch (op) {
              | Space =>
                [(Space, Var(NotInHole, NotInVarHole, hd))]
                @ mk_tl_seq(Comma, tl)
              | Comma =>
                [(Comma, Label(NotInLabelHole, hd))]
                @ mk_tl_seq(Space, bindings)
              | _ => failwith("are you really trying to build a record?")
              }
            };
          let seq = Seq.mk(Label(NotInLabelHole, hd), mk_tl_seq(Space, l));
          let skel = Skel.mk(precedence, associativity, seq);
          ExpLine(OpSeq(skel, seq));
        };
      let trimmed_def = def |> List.rev |> List.tl |> List.rev;
      Some(LetLine(p, None, trimmed_def @ [mk_struct_record(bindings)]));
    | None => None
    };
  | _ => None
  };

let rec set_duplicate_tuple_labels =
        (
          elements: list(skel),
          types: list(HTyp.t),
          seq: seq,
          u_gen: MetaVarGen.t,
          l: Label.t,
        )
        : (seq, list(HTyp.t), bool) => {
  assert(List.length(elements) == List.length(types));
  switch (elements, types) {
  | ([], _)
  | (_, []) => (seq, [], false)
  | ([skel, ...skels], [typ, ...types_tail]) =>
    let (seq, types_tail, changed) =
      set_duplicate_tuple_labels(skels, types_tail, seq, u_gen, l);
    switch (skel) {
    | BinOp(NotInHole, Space, Placeholder(n), _) =>
      let en = seq |> Seq.nth_operand(n);
      switch (en) {
      | Label(NotInLabelHole, l')
      | Label(InLabelHole(Duplicate, _), l') =>
        if (l == l') {
          let e_duplicate = Label(InLabelHole(Duplicate, u_gen), l);
          let seq = seq |> Seq.update_nth_operand(n, e_duplicate);
          (seq, [HTyp.Hole, ...types_tail], true);
        } else {
          (seq, [typ, ...types_tail], changed);
        }
      | _ => (seq, [typ, ...types_tail], changed)
      };
    | _ => (seq, [typ, ...types_tail], changed)
    };
  };
};
let rec find_and_set_dupe_labels_tuple =
        (
          elements: list(skel),
          types: list(HTyp.t),
          seq: seq,
          u_gen: MetaVarGen.t,
        )
        : (seq, list(HTyp.t)) => {
  assert(List.length(elements) == List.length(types));
  switch (elements, types) {
  | ([], _)
  | (_, []) => (seq, [])
  | ([skel, ...skels], [typ, ...types_tail]) =>
    switch (skel) {
    | BinOp(NotInHole, Space, Placeholder(n), _) =>
      let en = seq |> Seq.nth_operand(n);
      switch (en) {
      // Only set as a duplicate label for labels not already in a hole
      | Label(NotInLabelHole, l) =>
        let (seq, types_tail, changed) =
          set_duplicate_tuple_labels(skels, types_tail, seq, u_gen, l);
        if (changed) {
          let e_duplicate = Label(InLabelHole(Duplicate, u_gen), l);
          let seq = seq |> Seq.update_nth_operand(n, e_duplicate);
          let (seq, types_tail) =
            find_and_set_dupe_labels_tuple(skels, types_tail, seq, u_gen);
          (seq, [typ, ...types_tail]);
        } else {
          let (seq, types_tail) =
            find_and_set_dupe_labels_tuple(skels, types_tail, seq, u_gen);
          (seq, [typ, ...types_tail]);
        };
      | Label(InLabelHole(Duplicate, _), l) =>
        let (seq, types_tail, _) =
          set_duplicate_tuple_labels(skels, types_tail, seq, u_gen, l);
        let (seq, types_tail) =
          find_and_set_dupe_labels_tuple(skels, types_tail, seq, u_gen);
        (seq, [typ, ...types_tail]);
      // If no label not in a hole, continue recursing
      | _ =>
        let (seq, types_tail) =
          find_and_set_dupe_labels_tuple(skels, types_tail, seq, u_gen);
        (seq, [typ, ...types_tail]);
      };
    | _ =>
      let (seq, types_tail) =
        find_and_set_dupe_labels_tuple(skels, types_tail, seq, u_gen);
      (seq, [typ, ...types_tail]);
    }
  };
};

let rec find_and_clear_dupe_holes_labels_tuple =
        (
          elements: list(skel),
          types: list(HTyp.t),
          seq: seq,
          u_gen: MetaVarGen.t,
        )
        : (seq, list(HTyp.t)) => {
  assert(List.length(elements) == List.length(types));
  switch (elements, types) {
  | ([], _)
  | (_, []) => (seq, [])
  | ([skel, ...skels], [typ, ...types_tail]) =>
    switch (skel) {
    | BinOp(NotInHole, Space, Placeholder(n), _) =>
      let en = seq |> Seq.nth_operand(n);
      switch (en) {
      // Only set as a duplicate label for labels not already in a hole
      | Label(InLabelHole(Duplicate, _), l) =>
        let (seq, types_tail, changed) =
          set_duplicate_tuple_labels(skels, types_tail, seq, u_gen, l);
        if (changed) {
          let (seq, types_tail) =
            find_and_clear_dupe_holes_labels_tuple(
              skels,
              types_tail,
              seq,
              u_gen,
            );
          (seq, [typ, ...types_tail]);
        } else {
          let e_nohole = Label(NotInLabelHole, l);
          let seq = seq |> Seq.update_nth_operand(n, e_nohole);
          let (seq, types_tail) =
            find_and_clear_dupe_holes_labels_tuple(
              skels,
              types_tail,
              seq,
              u_gen,
            );
          (seq, [typ, ...types_tail]);
        };
      // If no label not in a hole, continue recursing
      | _ =>
        let (seq, types_tail) =
          find_and_clear_dupe_holes_labels_tuple(
            skels,
            types_tail,
            seq,
            u_gen,
          );
        (seq, [typ, ...types_tail]);
      };
    | _ =>
      let (seq, types_tail) =
        find_and_clear_dupe_holes_labels_tuple(skels, types_tail, seq, u_gen);
      (seq, [typ, ...types_tail]);
    }
  };
};

let new_InvalidText =
    (u_gen: MetaVarGen.t, t: string): (operand, MetaVarGen.t) => {
  let (u, u_gen) = MetaVarGen.next(u_gen);
  (InvalidText(u, t), u_gen);
};

/* helper function for constructing a new empty hole */
let new_EmptyHole = (u_gen: MetaVarGen.t): (operand, MetaVarGen.t) => {
  let (u, u_gen) = u_gen |> MetaVarGen.next;
  (EmptyHole(u), u_gen);
};

let is_EmptyHole =
  fun
  | EmptyHole(_) => true
  | _ => false;

let empty_rule = (u_gen: MetaVarGen.t): (rule, MetaVarGen.t) => {
  let (p, u_gen) = UHPat.new_EmptyHole(u_gen);
  let (e, u_gen) = new_EmptyHole(u_gen);
  let rule = Rule(OpSeq.wrap(p), Block.wrap(e));
  (rule, u_gen);
};

let rec get_err_status = (e: t): ErrStatus.t => get_err_status_block(e)
and get_err_status_block = block => {
  let (_, conclusion) = block |> Block.force_split_conclusion;
  conclusion |> get_err_status_opseq;
}
and get_err_status_opseq = opseq =>
  OpSeq.get_err_status(~get_err_status_operand, opseq)
and get_err_status_operand =
  fun
  | EmptyHole(_) => NotInHole
  | InvalidText(_, _) => NotInHole
  | Label(_, _) => NotInHole
  | Var(err, _, _)
  | IntLit(err, _)
  | FloatLit(err, _)
  | BoolLit(err, _)
  | ListNil(err)
  | Lam(err, _, _, _)
  | Inj(err, _, _)
  | Case(StandardErrStatus(err), _, _)
  | ApPalette(err, _, _, _)
  | Prj(err, _, _) => err
  | Case(InconsistentBranches(_), _, _) => NotInHole
  | Parenthesized(e) => get_err_status(e);

/* put e in the specified hole */
let rec set_err_status = (err: ErrStatus.t, e: t): t =>
  e |> set_err_status_block(err)
and set_err_status_block = (err: ErrStatus.t, block: block): block => {
  let (leading, conclusion) = block |> Block.force_split_conclusion;
  Block.join_conclusion(leading, conclusion |> set_err_status_opseq(err));
}
and set_err_status_opseq = (err, opseq) =>
  OpSeq.set_err_status(~set_err_status_operand, err, opseq)
and set_err_status_operand = (err, operand) =>
  switch (operand) {
  | EmptyHole(_) => operand
  | InvalidText(_, _) => operand
  | Label(_, _) => operand
  | Var(_, var_err, x) => Var(err, var_err, x)
  | IntLit(_, n) => IntLit(err, n)
  | FloatLit(_, f) => FloatLit(err, f)
  | BoolLit(_, b) => BoolLit(err, b)
  | ListNil(_) => ListNil(err)
  | Lam(_, p, ann, def) => Lam(err, p, ann, def)
  | Inj(_, inj_side, body) => Inj(err, inj_side, body)
  | Case(_, scrut, rules) => Case(StandardErrStatus(err), scrut, rules)
  | ApPalette(_, name, model, si) => ApPalette(err, name, model, si)
  | Parenthesized(body) => Parenthesized(body |> set_err_status(err))
  | Prj(_, body, label) => Prj(err, body, label)
  };

let is_inconsistent = operand =>
  switch (operand |> get_err_status_operand) {
  | InHole(TypeInconsistent, _) => true
  | _ => false
  };

/* put e in a new hole, if it is not already in a hole */
let rec mk_inconsistent = (u_gen: MetaVarGen.t, e: t): (t, MetaVarGen.t) =>
  mk_inconsistent_block(u_gen, e)
and mk_inconsistent_block =
    (u_gen: MetaVarGen.t, block: block): (block, MetaVarGen.t) => {
  let (leading, conclusion) = block |> Block.force_split_conclusion;
  let (conclusion, u_gen) = conclusion |> mk_inconsistent_opseq(u_gen);
  (Block.join_conclusion(leading, conclusion), u_gen);
}
and mk_inconsistent_opseq = (u_gen, opseq) =>
  OpSeq.mk_inconsistent(~mk_inconsistent_operand, u_gen, opseq)
and mk_inconsistent_operand = (u_gen, operand) =>
  switch (operand) {
  /* already in hole */
  | EmptyHole(_)
  | InvalidText(_, _)
  | Label(_, _)
  | Var(InHole(TypeInconsistent, _), _, _)
  | IntLit(InHole(TypeInconsistent, _), _)
  | FloatLit(InHole(TypeInconsistent, _), _)
  | BoolLit(InHole(TypeInconsistent, _), _)
  | ListNil(InHole(TypeInconsistent, _))
  | Lam(InHole(TypeInconsistent, _), _, _, _)
  | Inj(InHole(TypeInconsistent, _), _, _)
  | Case(StandardErrStatus(InHole(TypeInconsistent, _)), _, _)
  | ApPalette(InHole(TypeInconsistent, _), _, _, _)
  | Prj(InHole(TypeInconsistent, _), _, _) => (operand, u_gen)
  /* not in hole */
  | Var(NotInHole | InHole(WrongLength, _), _, _)
  | IntLit(NotInHole | InHole(WrongLength, _), _)
  | FloatLit(NotInHole | InHole(WrongLength, _), _)
  | BoolLit(NotInHole | InHole(WrongLength, _), _)
  | ListNil(NotInHole | InHole(WrongLength, _))
  | Lam(NotInHole | InHole(WrongLength, _), _, _, _)
  | Inj(NotInHole | InHole(WrongLength, _), _, _)
  | Case(
      StandardErrStatus(NotInHole | InHole(WrongLength, _)) |
      InconsistentBranches(_, _),
      _,
      _,
    )
  | ApPalette(NotInHole | InHole(WrongLength, _), _, _, _)
  | Prj(NotInHole | InHole(WrongLength, _), _, _) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    let operand =
      operand |> set_err_status_operand(InHole(TypeInconsistent, u));
    (operand, u_gen);
  /* err in constructor args */
  | Parenthesized(body) =>
    let (body, u_gen) = body |> mk_inconsistent(u_gen);
    (Parenthesized(body), u_gen);
  };

let text_operand =
    (u_gen: MetaVarGen.t, shape: TextShape.t): (operand, MetaVarGen.t) =>
  switch (shape) {
  | Underscore => (var("_"), u_gen)
  | IntLit(n) => (intlit(n), u_gen)
  | FloatLit(f) => (floatlit(f), u_gen)
  | BoolLit(b) => (boollit(b), u_gen)
  | Var(x) => (var(x), u_gen)
  | ExpandingKeyword(kw) =>
    let (u, u_gen) = u_gen |> MetaVarGen.next;
    (
      var(~var_err=InVarHole(Free, u), kw |> ExpandingKeyword.to_string),
      u_gen,
    );
  | Label(l) => (label(l), u_gen)
  | InvalidTextShape(t) => new_InvalidText(u_gen, t)
  };

let associate =
  Skel.mk(Operators_Exp.precedence, Operators_Exp.associativity);

let mk_OpSeq = OpSeq.mk(~associate);

let rec is_complete_line = (l: line, check_type_holes: bool): bool => {
  switch (l) {
  | EmptyLine
  | CommentLine(_) => true
  | LetLine(pat, option_ty, body) =>
    if (check_type_holes) {
      switch (option_ty) {
      | None => UHPat.is_complete(pat) && is_complete(body, check_type_holes)
      | Some(ty) =>
        UHPat.is_complete(pat)
        && is_complete(body, check_type_holes)
        && UHTyp.is_complete(ty)
      };
    } else {
      UHPat.is_complete(pat) && is_complete(body, check_type_holes);
    }
  | ExpLine(body) =>
    OpSeq.is_complete(is_complete_operand, body, check_type_holes)
  | StructLine(p, _, def) =>
    UHPat.is_complete(p) && is_complete(def, check_type_holes)
  };
}
and is_complete_block = (b: block, check_type_holes: bool): bool => {
  b |> List.for_all(l => is_complete_line(l, check_type_holes));
}
and is_complete_rule = (rule: rule, check_type_holes: bool): bool => {
  switch (rule) {
  | Rule(pat, body) =>
    UHPat.is_complete(pat) && is_complete(body, check_type_holes)
  };
}
and is_complete_rules = (rules: rules, check_type_holes: bool): bool => {
  rules |> List.for_all(l => is_complete_rule(l, check_type_holes));
}
and is_complete_operand = (operand: 'operand, check_type_holes: bool): bool => {
  switch (operand) {
  | EmptyHole(_) => false
  | InvalidText(_, _) => false
  | Var(InHole(_), _, _) => false
  | Label(_, _) => false
  | Var(NotInHole, InVarHole(_), _) => false
  | Var(NotInHole, NotInVarHole, _) => true
  | IntLit(InHole(_), _) => false
  | IntLit(NotInHole, _) => true
  | FloatLit(InHole(_), _) => false
  | FloatLit(NotInHole, _) => true
  | BoolLit(InHole(_), _) => false
  | BoolLit(NotInHole, _) => true
  | ListNil(InHole(_)) => false
  | ListNil(NotInHole) => true
  | Lam(InHole(_), _, _, _) => false
  | Lam(NotInHole, pat, option_ty, body) =>
    if (check_type_holes) {
      switch (option_ty) {
      | None => UHPat.is_complete(pat) && is_complete(body, check_type_holes)
      | Some(ty) =>
        UHPat.is_complete(pat)
        && is_complete(body, check_type_holes)
        && UHTyp.is_complete(ty)
      };
    } else {
      UHPat.is_complete(pat) && is_complete(body, check_type_holes);
    }
  | Inj(InHole(_), _, _) => false
  | Inj(NotInHole, _, body) => is_complete(body, check_type_holes)
  | Case(StandardErrStatus(InHole(_)) | InconsistentBranches(_), _, _) =>
    false
  | Case(StandardErrStatus(NotInHole), body, rules) =>
    is_complete(body, check_type_holes)
    && is_complete_rules(rules, check_type_holes)
  | Parenthesized(body) => is_complete(body, check_type_holes)
  | ApPalette(InHole(_), _, _, _) => false
  | ApPalette(NotInHole, _, _, _) => failwith("unimplemented")
  | Prj(InHole(_), _, _) => false
  | Prj(NotInHole, op, _) => is_complete_operand(op, check_type_holes)
  };
}
and is_complete = (exp: t, check_type_holes: bool): bool => {
  is_complete_block(exp, check_type_holes);
};
