open Sexplib.Std;
open GeneralUtil;

[@deriving sexp]
type typed =
  /* cursor in analytic position */
  // cursor is on a lambda with an argument type annotation
  | AnaAnnotatedLambda(HTyp.t, HTyp.t)
  // cursor is on a type inconsistent expression
  | AnaTypeInconsistent(HTyp.t, HTyp.t)
  // cursor is on a tuple of the wrong length
  | AnaWrongLength
      // expected length
      (
        int,
        // got length
        int,
        // expected type
        HTyp.t,
      )
  // cursor is on a free variable
  | AnaFree(HTyp.t)
  // cursor is on a keyword
  | AnaKeyword(HTyp.t, Keyword.t)
  // none of the above and didn't go through subsumption
  | Analyzed(HTyp.t)
  // none of the above and went through subsumption
  | AnaSubsumed(HTyp.t, HTyp.t)
  /* cursor in synthetic position */
  // cursor is on the function position of an ap,
  // and that expression does not synthesize a type
  // with a matched arrow type
  | SynErrorArrow
      // expected
      (
        HTyp.t,
        // got
        HTyp.t,
      )
  // cursor is on the function position of an ap,
  // and that expression does synthesize a type
  // with a matched arrow type
  | SynMatchingArrow(HTyp.t, HTyp.t)
  // cursor is on a free variable in the function
  // position of an ap
  | SynFreeArrow(HTyp.t)
  // cursor is on a keyword in the function position of an ap
  | SynKeywordArrow(HTyp.t, Keyword.t)
  // none of the above, cursor is on a free variable
  | SynFree
  // cursor is on a keyword
  | SynKeyword(Keyword.t)
  // none of the above
  | Synthesized(HTyp.t)
  /* cursor in analytic pattern position */
  // cursor is on a type inconsistent pattern
  | PatAnaTypeInconsistent(HTyp.t, HTyp.t)
  // cursor is on a tuple pattern of the wrong length
  | PatAnaWrongLength
      // expected length
      (
        int,
        // got length
        int,
        // expected type
        HTyp.t,
      )
  // cursor is on a keyword
  | PatAnaKeyword(HTyp.t, Keyword.t)
  // none of the above and didn't go through subsumption
  | PatAnalyzed(HTyp.t)
  // none of the above and went through subsumption
  | PatAnaSubsumed(HTyp.t, HTyp.t)
  /* cursor in synthetic pattern position */
  // cursor is on a keyword
  | PatSynthesized(HTyp.t)
  | PatSynKeyword(Keyword.t)
  /* cursor in type position */
  | OnType
  /* (we will have a richer structure here later)*/
  | OnLine
  | OnRule;

[@deriving sexp]
type t = {
  typed,
  ctx: Contexts.t,
};

let mk_cursor_info = (typed, ctx) => {typed, ctx};

/*
 [@deriving sexp]
 type child_indices = list(int);

 let child_indices_of_current_node = ci =>
   switch (ci.node) {
   | Line(li) => UHExp.child_indices_line(li)
   | Exp(e) => UHExp.child_indices_operand(e)
   | Rule(rule) => UHExp.child_indices_rule(rule)
   | Pat(p) => UHPat.child_indices(p)
   | Typ(ty) => UHTyp.child_indices(ty)
   };

 let preserved_child_term_of_node = ci =>
   switch (ci.node) {
   | Line(li) =>
     li
     |> UHExp.favored_child_of_line
     |> Opt.map_default(
          ~default=None,
          fun
          | (_, UHExp.Block([], EmptyHole(_))) => None
          | (i, block) => Some((i, Expression(block))),
        )
   | Exp(e) =>
     switch (e |> UHExp.favored_child_of_operand, ci.frame) {
     | (None, _) => None
     | (_, TypFrame(_) | PatFrame(_)) => None
     | (Some((_, Block([], EmptyHole(_)))), _) => None
     | (Some((i, block)), ExpFrame(_, None, _)) =>
       Some((i, Expression(block)))
     | (
         Some((i, Block([], OpSeq(_, _)) as block)),
         ExpFrame(_, Some(_surround), _),
       ) =>
       Some((i, Expression(block)))
     | (
         Some((i, Block([], conclusion) as block)),
         ExpFrame(_, Some(_surround), _),
       ) =>
       switch (e, conclusion |> UHExp.bidelimited) {
       | (Parenthesized(_), false) => None
       | _ => Some((i, Expression(block)))
       }
     | (Some((_, Block(_, _))), ExpFrame(_, Some(_surround), _)) => None
     }
   | Pat(p) =>
     p |> UHPat.favored_child |> Opt.map(((i, p)) => (i, Pattern(p)))
   | Typ(ty) =>
     ty |> UHTyp.favored_child |> Opt.map(((i, ty)) => (i, Type(ty)))
   | Rule(_) => None
   };
 */

module Typ = {
  let cursor_info = (ctx: Contexts.t, zty: ZTyp.t): option(t) =>
    Some(mk_cursor_info(OnType, ctx));
};

module Pat = {
  let erase_zseq =
    ZSeq.erase(
      ~erase_zoperand=ZPat.erase_zoperand,
      ~erase_zoperator=ZPat.erase_zoperator,
    );

  let rec syn_cursor_info = (ctx: Contexts.t, zp: ZPat.t): option(t) =>
    syn_cursor_info_zopseq(ctx, zp)
  and syn_cursor_info_zopseq =
      (ctx: Contexts.t, ZOpSeq(skel, zseq): ZPat.zopseq): option(t) => {
    // handle n-tuples
    let skels = skel |> UHPat.get_tuple_elements;
    switch (zseq) {
    | ZOperator((_, Comma), _) =>
      // cursor on tuple comma:
      // manually synthesize types for each tuple element
      // and put together so that an "inner" comma doesn't
      // synthesize a smaller tuple type (as would happen
      // if we defer to syn_cursor_info_skel)
      skels
      |> List.fold_left(
           (acc: option((list(HTyp.t), Contexts.t)), skel) =>
             switch (acc) {
             | None => None
             | Some((rev_tys, ctx)) =>
               switch (Statics.Pat.syn_skel(ctx, skel, zseq |> erase_zseq)) {
               | None => None
               | Some((ty, ctx)) => Some(([ty, ...rev_tys], ctx))
               }
             },
           Some(([], ctx)),
         )
      |> Opt.map(((rev_tys, _)) =>
           mk_cursor_info(
             PatSynthesized(rev_tys |> List.rev |> HTyp.make_tuple),
             ctx,
           )
         )
    | _ =>
      // cursor within tuple element:
      // accumulate ctx over skels until skel containing
      // cursor found, then defer to syn_cursor_info_skel
      skels
      |> List.fold_left(
           (acc: option((Contexts.t, option(UHPat.skel))), skel) =>
             switch (acc) {
             | None as failed => failed
             | Some((_, Some(_))) as found_cursor => found_cursor
             | Some((ctx, None)) =>
               if (ZOpSeq.skel_contains_cursor(skel, zseq)) {
                 Some((ctx, Some(skel)));
               } else {
                 switch (Statics.Pat.syn_skel(ctx, skel, zseq |> erase_zseq)) {
                 | None => None
                 | Some((_, ctx)) => Some((ctx, None))
                 };
               }
             },
           Some((ctx, None)),
         )
      |> (
        fun
        | None
        | Some((_, None)) => None
        | Some((ctx, Some(skel))) => syn_cursor_info_skel(ctx, skel, zseq)
      )
    };
  }
  and syn_cursor_info_skel =
      (ctx: Contexts.t, skel: UHPat.skel, zseq: ZPat.zseq): option(t) =>
    // recurse through skel to find subskel corresponding to cursor
    switch (skel) {
    | Placeholder(n) =>
      switch (zseq) {
      | ZOperand(zoperand, (prefix, _))
          when n == Seq.length_of_affix(prefix) - 1 =>
        // found cursor
        syn_cursor_info_zoperand(ctx, zoperand)
      | _ => None
      }
    | BinOp(_, op, skel1, skel2) =>
      let seq = zseq |> ZPat.erase_zseq;
      switch (zseq) {
      | ZOperator(_, (prefix, _))
          when Skel.rightmost_tm_index(skel1) == Seq.length(prefix) - 1 =>
        // found cursor
        Statics.Pat.syn_skel(ctx, skel, seq)
        |> Opt.map(((ty, _)) => mk_cursor_info(PatSynthesized(ty), ctx))
      | _ =>
        // recurse
        switch (op) {
        | Comma =>
          switch (syn_cursor_info_skel(ctx, skel1, zseq)) {
          | Some(_) as res => res
          | None =>
            switch (Statics.Pat.syn_skel(ctx, skel1, seq)) {
            | None => None
            | Some((_, ctx)) => syn_cursor_info_skel(ctx, skel2, zseq)
            }
          }
        | Space =>
          switch (ana_cursor_info_skel(ctx, skel1, zseq, HTyp.Hole)) {
          | Some(_) as res => res
          | None =>
            switch (Statics.Pat.ana_skel(ctx, skel1, seq, Hole)) {
            | None => None
            | Some(ctx) => ana_cursor_info_skel(ctx, skel2, zseq, Hole)
            }
          }
        | Cons =>
          switch (syn_cursor_info_skel(ctx, skel1, zseq)) {
          | Some(_) as res => res
          | None =>
            switch (Statics.Pat.syn_skel(ctx, skel1, seq)) {
            | None => None
            | Some((ty_elt, ctx)) =>
              ana_cursor_info_skel(ctx, skel2, zseq, HTyp.List(ty_elt))
            }
          }
        }
      };
    }
  and syn_cursor_info_zoperand =
      (ctx: Contexts.t, zoperand: ZPat.zoperand): option(t) =>
    switch (zoperand) {
    | CursorP(_, Var(_, InVarHole(Keyword(k), _), _)) =>
      Some(mk_cursor_info(PatSynKeyword(k), ctx))
    | CursorP(_, p) =>
      Statics.Pat.syn_operand(ctx, p)
      |> Opt.map(((ty, _)) => mk_cursor_info(PatSynthesized(ty), ctx))
    | InjZ(_, _, zbody)
    | ParenthesizedZ(zbody) => syn_cursor_info(ctx, zbody)
    }
  and ana_cursor_info = (ctx: Contexts.t, zp: ZPat.t, ty: HTyp.t): option(t) =>
    ana_cursor_info_zopseq(ctx, zp, ty)
  and ana_cursor_info_zopseq =
      (
        ctx: Contexts.t,
        ZOpSeq(skel, zseq) as zopseq: ZPat.zopseq,
        ty: HTyp.t,
      )
      : option(t) => {
    // handle n-tuples
    let skels = skel |> UHPat.get_tuple_elements;
    let tys = ty |> HTyp.get_tuple_elements;
    switch (opt_zip(skels, tys)) {
    | Some(skel_tys) =>
      switch (zseq) {
      | ZOperator((_, Comma), _) =>
        // cursor on tuple comma
        skel_tys
        |> List.fold_left(
             (acc: option(Contexts.t), (skel, ty)) =>
               switch (acc) {
               | None => None
               | Some(ctx) =>
                 Statics.Pat.ana_skel(ctx, skel, zseq |> erase_zseq, ty)
               },
             Some(ctx),
           )
        |> Opt.map(_ => mk_cursor_info(PatAnalyzed(ty), ctx))
      | _ =>
        // cursor in tuple element:
        // accumultate ctx over skels until skel containing
        // cursor found, then defer to ana_cursor_info_skel
        skel_tys
        |> List.fold_left(
             (acc: option((Contexts.t, option(UHPat.skel))), (skel, ty)) =>
               switch (acc) {
               | None as failed => failed
               | Some((_, Some(_))) as found_cursor => found_cursor
               | Some((ctx, None)) =>
                 if (ZOpSeq.skel_contains_cursor(skel, zseq)) {
                   Some((ctx, Some(skel)));
                 } else {
                   switch (
                     Statics.Pat.ana_skel(ctx, skel, zseq |> erase_zseq, ty)
                   ) {
                   | None => None
                   | Some(ctx) => Some((ctx, None))
                   };
                 }
               },
             Some((ctx, None)),
           )
        |> (
          fun
          | None
          | Some((_, None)) => None
          | Some((ctx, Some(skel))) =>
            ana_cursor_info_skel(ctx, skel, zseq, ty)
        )
      }
    | None =>
      switch (skels, tys) {
      | ([Placeholder(n)], _) =>
        switch (zseq) {
        | ZOperator(_) => assert(false)
        | ZOperand(zoperand, _) =>
          ana_cursor_info_zoperand(ctx, zoperand, ty)
        }
      | (_, [Hole]) =>
        skels
        |> List.fold_left(
             (acc: option((Contexts.t, option(UHPat.skel))), skel) =>
               switch (acc) {
               | None as failed => failed
               | Some((_, Some(_))) as found_cursor => found_cursor
               | Some((ctx, None)) =>
                 if (ZOpSeq.skel_contains_cursor(skel, zseq)) {
                   Some((ctx, Some(skel)));
                 } else {
                   switch (
                     Statics.Pat.ana_skel(ctx, skel, zseq |> erase_zseq, Hole)
                   ) {
                   | None => None
                   | Some(ctx) => Some((ctx, None))
                   };
                 }
               },
             Some((ctx, None)),
           )
        |> (
          fun
          | None
          | Some((_, None)) => None
          | Some((ctx, Some(skel))) =>
            ana_cursor_info_skel(ctx, skel, zseq, Hole)
        )
      | _ =>
        let opseq = zopseq |> ZPat.erase_zopseq;
        switch (opseq |> UHPat.get_err_status_opseq) {
        | NotInHole
        | InHole(TypeInconsistent, _) => None
        | InHole(WrongLength, _) =>
          let opseq' = opseq |> UHPat.set_err_status_opseq(NotInHole);
          Statics.Pat.syn_opseq(ctx, opseq')
          |> Opt.map(_ =>
               mk_cursor_info(
                 PatAnaWrongLength(
                   tys |> List.length,
                   skels |> List.length,
                   ty,
                 ),
                 ctx,
               )
             );
        };
      }
    };
  }
  and ana_cursor_info_skel =
      (ctx: Contexts.t, skel: UHPat.skel, zseq: ZPat.zseq, ty: HTyp.t)
      : option(t) =>
    // recurse through skel to find subskel corresponding to cursor
    switch (skel) {
    | Placeholder(n) =>
      switch (zseq) {
      | ZOperand(zoperand, (prefix, _))
          when n == Seq.length_of_affix(prefix) - 1 =>
        // found cursor
        ana_cursor_info_zoperand(ctx, zoperand, ty)
      | _ => None
      }
    | BinOp(err, op, skel1, skel2) =>
      let seq = zseq |> ZPat.erase_zseq;
      switch (zseq) {
      | ZOperator(_, (prefix, _))
          when Skel.rightmost_tm_index(skel1) == Seq.length(prefix) - 1 =>
        // found cursor
        switch (err) {
        | NotInHole =>
          switch (Statics.Pat.ana_skel(ctx, skel, seq, ty)) {
          | None => None
          | Some(_) => Some(mk_cursor_info(PatAnalyzed(ty), ctx))
          }
        | InHole(reason, _) =>
          switch (Statics.Pat.syn_skel(ctx, skel, seq)) {
          | None => None
          | Some((syn_ty, _)) =>
            switch (reason) {
            | WrongLength =>
              // tuples handled at opseq level
              None
            | TypeInconsistent =>
              Some(mk_cursor_info(PatAnaTypeInconsistent(ty, syn_ty), ctx))
            }
          }
        }
      | _ =>
        // recurse
        switch (err) {
        | NotInHole =>
          switch (op) {
          | Comma =>
            // tuples handled at opseq level
            None
          | Space =>
            switch (ana_cursor_info_skel(ctx, skel1, zseq, Hole)) {
            | Some(_) as res => res
            | None =>
              switch (Statics.Pat.ana_skel(ctx, skel1, seq, Hole)) {
              | None => None
              | Some(ctx) => ana_cursor_info_skel(ctx, skel2, zseq, Hole)
              }
            }
          | Cons =>
            switch (HTyp.matched_list(ty)) {
            | None => None
            | Some(ty_elt) =>
              switch (ana_cursor_info_skel(ctx, skel1, zseq, ty_elt)) {
              | Some(_) as res => res
              | None =>
                switch (Statics.Pat.ana_skel(ctx, skel1, seq, ty_elt)) {
                | None => None
                | Some(ctx) =>
                  ana_cursor_info_skel(ctx, skel2, zseq, List(ty_elt))
                }
              }
            }
          }
        | InHole(_) => syn_cursor_info_skel(ctx, skel, zseq)
        }
      };
    }
  and ana_cursor_info_zoperand =
      (ctx: Contexts.t, zoperand: ZPat.zoperand, ty: HTyp.t): option(t) =>
    switch (zoperand) {
    | CursorP(_, operand) =>
      switch (operand) {
      // in hole
      | EmptyHole(_) => Some(mk_cursor_info(PatAnaSubsumed(ty, Hole), ctx))
      | Wild(InHole(TypeInconsistent, _))
      | Var(InHole(TypeInconsistent, _), _, _)
      | NumLit(InHole(TypeInconsistent, _), _)
      | BoolLit(InHole(TypeInconsistent, _), _)
      | ListNil(InHole(TypeInconsistent, _))
      | Inj(InHole(TypeInconsistent, _), _, _) =>
        let operand' = UHPat.set_err_status_operand(NotInHole, operand);
        switch (Statics.Pat.syn_operand(ctx, operand')) {
        | None => None
        | Some((ty', _)) =>
          Some(mk_cursor_info(PatAnaTypeInconsistent(ty, ty'), ctx))
        };
      | Wild(InHole(WrongLength, _))
      | Var(InHole(WrongLength, _), _, _)
      | NumLit(InHole(WrongLength, _), _)
      | BoolLit(InHole(WrongLength, _), _)
      | ListNil(InHole(WrongLength, _))
      | Inj(InHole(WrongLength, _), _, _) => None
      | Var(NotInHole, InVarHole(Keyword(k), _), _) =>
        Some(mk_cursor_info(PatAnaKeyword(ty, k), ctx))
      // not in hole
      | Wild(NotInHole)
      | Var(NotInHole, _, _)
      | ListNil(NotInHole) => Some(mk_cursor_info(PatAnalyzed(ty), ctx))
      | NumLit(NotInHole, _) =>
        Some(mk_cursor_info(PatAnaSubsumed(ty, Num), ctx))
      | BoolLit(NotInHole, _) =>
        Some(mk_cursor_info(PatAnaSubsumed(ty, Bool), ctx))
      | Inj(NotInHole, _, _) => Some(mk_cursor_info(PatAnalyzed(ty), ctx))
      | Parenthesized(body) =>
        Statics.Pat.ana(ctx, body, ty)
        |> Opt.map(_ => mk_cursor_info(PatAnalyzed(ty), ctx))
      }
    | InjZ(InHole(WrongLength, _), _, _) => None
    | InjZ(InHole(TypeInconsistent, _), _, _) =>
      syn_cursor_info_zoperand(ctx, zoperand)
    | InjZ(NotInHole, position, zbody) =>
      switch (HTyp.matched_sum(ty)) {
      | None => None
      | Some((tyL, tyR)) =>
        let ty_body = InjSide.pick(position, tyL, tyR);
        ana_cursor_info(ctx, zbody, ty_body);
      }
    | ParenthesizedZ(zbody) => ana_cursor_info(ctx, zbody, ty)
    };
};

module Exp = {
  let rec _ana_cursor_found_block =
          (ctx: Contexts.t, Block(lines, e): UHExp.block, ty: HTyp.t)
          : option(typed) =>
    switch (Statics.syn_lines(ctx, lines)) {
    | None => None
    | Some(ctx) =>
      switch (_ana_cursor_found_exp(ctx, e, ty)) {
      | None => None
      | Some((typed, _, _)) => Some(typed)
      }
    }
  and _ana_cursor_found_exp =
      (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t)
      : option((typed, node, Contexts.t)) =>
    switch (e) {
    /* in hole */
    | Var(InHole(TypeInconsistent, _), _, _)
    | NumLit(InHole(TypeInconsistent, _), _)
    | BoolLit(InHole(TypeInconsistent, _), _)
    | ListNil(InHole(TypeInconsistent, _))
    | Lam(InHole(TypeInconsistent, _), _, _, _)
    | Inj(InHole(TypeInconsistent, _), _, _)
    | Case(InHole(TypeInconsistent, _), _, _, _)
    | ApPalette(InHole(TypeInconsistent, _), _, _, _)
    | OpSeq(BinOp(InHole(TypeInconsistent, _), _, _, _), _) =>
      let e_nih = UHExp.set_err_status_operand(NotInHole, e);
      switch (Statics.Exp.syn_operand(ctx, e_nih)) {
      | None => None
      | Some(ty') => Some((AnaTypeInconsistent(ty, ty'), Exp(e), ctx))
      };
    | Var(InHole(WrongLength, _), _, _)
    | NumLit(InHole(WrongLength, _), _)
    | BoolLit(InHole(WrongLength, _), _)
    | ListNil(InHole(WrongLength, _)) => None
    | Lam(InHole(WrongLength, _), _, _, _)
    | Inj(InHole(WrongLength, _), _, _)
    | Case(InHole(WrongLength, _), _, _, _)
    | ApPalette(InHole(WrongLength, _), _, _, _) => None
    | OpSeq(BinOp(InHole(reason, _), Comma, skel1, skel2), _) =>
      switch (ty, reason) {
      | (Prod(ty1, ty2), WrongLength) =>
        let n_elts = ListMinTwo.length(UHExp.get_tuple(skel1, skel2));
        let n_types = ListMinTwo.length(HTyp.get_tuple(ty1, ty2));
        Some((AnaWrongLength(n_types, n_elts, ty), Exp(e), ctx));
      | (_, TypeInconsistent) =>
        let n_elts = ListMinTwo.length(UHExp.get_tuple(skel1, skel2));
        Some((AnaWrongLength(1, n_elts, ty), Exp(e), ctx));
      | _ => None
      }
    | OpSeq(BinOp(InHole(WrongLength, _), _, _, _), _) => None
    /* not in hole */
    | Var(_, InVarHole(Keyword(k), _), _) =>
      Some((AnaKeyword(ty, k), Exp(e), ctx))
    | Var(_, InVarHole(Free, _), _) => Some((AnaFree(ty), Exp(e), ctx))
    | ListNil(NotInHole) => Some((Analyzed(ty), Exp(e), ctx))
    | EmptyHole(_)
    | Var(NotInHole, NotInVarHole, _)
    | NumLit(NotInHole, _)
    | BoolLit(NotInHole, _) =>
      switch (Statics.Exp.syn_operand(ctx, e)) {
      | None => None
      | Some(ty') => Some((AnaSubsumed(ty, ty'), Exp(e), ctx))
      }
    | Case(NotInHole, _, _, _) => Some((Analyzed(ty), Exp(e), ctx))
    | Lam(NotInHole, _, ann, _) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => None
      | Some((ty1_given, ty2)) =>
        switch (ann) {
        | Some(uty1) =>
          let ty1_ann = UHTyp.expand(uty1);
          switch (HTyp.consistent(ty1_ann, ty1_given)) {
          | false => None
          | true =>
            Some((
              AnaAnnotatedLambda(ty, Arrow(ty1_ann, ty2)),
              Exp(e),
              ctx,
            ))
          };
        | None => Some((Analyzed(ty), Exp(e), ctx))
        }
      }
    | Inj(NotInHole, _, _) => Some((Analyzed(ty), Exp(e), ctx))
    | OpSeq(BinOp(NotInHole, Comma, _, _), _)
    | OpSeq(BinOp(NotInHole, Cons, _, _), _) =>
      Some((Analyzed(ty), Exp(e), ctx))
    | OpSeq(Placeholder(_), _) => None
    | OpSeq(BinOp(NotInHole, And | Or, _, _), _)
    | OpSeq(BinOp(NotInHole, Minus, _, _), _)
    | OpSeq(BinOp(NotInHole, Plus, _, _), _)
    | OpSeq(BinOp(NotInHole, Times, _, _), _)
    | OpSeq(BinOp(NotInHole, LessThan, _, _), _)
    | OpSeq(BinOp(NotInHole, GreaterThan, _, _), _)
    | OpSeq(BinOp(NotInHole, Equals, _, _), _)
    | OpSeq(BinOp(NotInHole, Space, _, _), _)
    | ApPalette(NotInHole, _, _, _) =>
      switch (Statics.Exp.syn_operand(ctx, e)) {
      | None => None
      | Some(ty') => Some((AnaSubsumed(ty, ty'), Exp(e), ctx))
      }
    | Parenthesized(block) =>
      switch (_ana_cursor_found_block(ctx, block, ty)) {
      | None => None
      | Some(typed) => Some((typed, Exp(e), ctx))
      }
    };

  let ana_cursor_found_exp =
      (ctx: Contexts.t, e: UHExp.t, ty: HTyp.t, cursor: CursorPosition.t)
      : option(t) =>
    switch (_ana_cursor_found_exp(ctx, e, ty)) {
    | None => None
    | Some((mode, sort, ctx)) =>
      let (prefix, surround, suffix) = frame;
      Some(
        mk_cursor_info(
          ~subskel_range=?
            switch (cursor, e) {
            | (OnDelim(k, _), OpSeq(skel, _)) =>
              Some(Skel.range_of_subskel_rooted_at_op(k, skel))
            | _ => None
            },
          mode,
          sort,
          ExpFrame(prefix, surround, suffix),
          cursor,
          ctx,
          node_steps,
          term_steps,
        ),
      );
    };

  let rec syn_cursor_info = (ctx: Contexts.t, ze: ZExp.t): option(t) =>
    syn_cursor_info_block(ctx, ze)
  and syn_cursor_info_block =
      (ctx: Contexts.t, zblock: ZExp.zblock): option(t) =>
    switch (zblock) {
    | BlockZL((prefix, zline, suffix), conclusion) =>
      switch (Statics.syn_lines(ctx, prefix)) {
      | None => None
      | Some(ctx) =>
        syn_cursor_info_line(~line_no=List.length(prefix), ctx, zline)
      }
    | BlockZE(lines, ze) =>
      switch (Statics.syn_lines(ctx, lines)) {
      | None => None
      | Some(ctx) => syn_cursor_info(ctx, ze)
      }
    }
  and syn_cursor_info_line =
      (~line_no, ctx: Contexts.t, zli: ZExp.zline): option(t) =>
    switch (zli) {
    | CursorL(cursor, line) =>
      let (prefix, _, suffix) = frame;
      Some(
        mk_cursor_info(
          ~subblock_start=line_no,
          OnLine,
          Line(line),
          ExpFrame(prefix, None, suffix),
          cursor,
          ctx,
          node_steps,
          term_steps,
        ),
      );
    | ExpLineZ(ze) => syn_cursor_info(ctx, ze)
    | LetLineZP(zp, ann, block) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1 = UHTyp.expand(uty1);
        ana_cursor_info(~node_steps, ~term_steps, ctx, zp, ty1);
      | None =>
        switch (Statics.syn_block(ctx, block)) {
        | None => None
        | Some(ty1) => ana_cursor_info(~node_steps, ~term_steps, ctx, zp, ty1)
        }
      }
    | LetLineZA(_, zann, _) =>
      cursor_info_typ(~node_steps, ~term_steps, ctx, zann)
    | LetLineZE(p, ann, zblock) =>
      switch (ann) {
      | Some(uty1) =>
        let ty1 = UHTyp.expand(uty1);
        let ctx1 =
          Statics.ctx_for_let(ctx, p, ty1, ZExp.erase_zblock(zblock));
        ana_cursor_info_block(~node_steps, ~term_steps, ctx1, zblock, ty1);
      | None => syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock)
      }
    }
  and syn_cursor_info_opseq =
      (ctx: Contexts.t, zopseq: ZExp.zopseq): option(t) =>
    failwith("unimplemented")
  and syn_cursor_info_operand = (ctx: Contexts.t, ze: ZExp.t): option(t) => {
    let (prefix, surround, suffix) = frame;
    switch (ze) {
    | CursorE(cursor, Var(_, InVarHole(Keyword(k), _), _) as e) =>
      Some(
        mk_cursor_info(
          SynKeyword(k),
          Exp(e),
          ExpFrame(prefix, surround, suffix),
          cursor,
          ctx,
          node_steps,
          term_steps,
        ),
      )
    | CursorE(cursor, Var(_, InVarHole(Free, _), _) as e) =>
      Some(
        mk_cursor_info(
          SynFree,
          Exp(e),
          ExpFrame(prefix, surround, suffix),
          cursor,
          ctx,
          node_steps,
          term_steps,
        ),
      )
    | CursorE(cursor, e) =>
      switch (Statics.Exp.syn_operand(ctx, e)) {
      | None => None
      | Some(ty) =>
        Some(
          mk_cursor_info(
            ~subskel_range=?
              switch (cursor, e) {
              | (OnDelim(k, _), OpSeq(skel, _)) =>
                Some(Skel.range_of_subskel_rooted_at_op(k, skel))
              | _ => None
              },
            Synthesized(ty),
            Exp(e),
            ExpFrame(prefix, surround, suffix),
            cursor,
            ctx,
            node_steps,
            term_steps,
          ),
        )
      }
    | OpSeqZ(skel, ze0, surround) =>
      let e0 = ZExp.erase_zoperand(ze0);
      let seq = Seq.t_of_operand_and_surround(e0, surround);
      let n = Seq.surround_prefix_length(surround);
      syn_cursor_info_skel(
        ~node_steps,
        ~term_steps,
        ~frame=(prefix, Some(surround), suffix),
        ctx,
        skel,
        seq,
        n,
        ze0,
      );
    | ParenthesizedZ(zblock) =>
      syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock)
    | LamZP(_, zp, ann, _) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => Hole
        };
      ana_cursor_info(~node_steps, ~term_steps, ctx, zp, ty1);
    | LamZA(_, _, zann, _) =>
      cursor_info_typ(~node_steps, ~term_steps, ctx, zann)
    | LamZE(_, p, ann, zblock) =>
      let ty1 =
        switch (ann) {
        | Some(uty1) => UHTyp.expand(uty1)
        | None => Hole
        };
      switch (Statics.Pat.ana(ctx, p, ty1)) {
      | None => None
      | Some(ctx1) =>
        syn_cursor_info_block(~node_steps, ~term_steps, ctx1, zblock)
      };
    | InjZ(_, _, zblock) =>
      syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock)
    | CaseZE(_, _, _, None)
    | CaseZR(_, _, _, None) => None
    | CaseZE(_, zblock, _, Some(_)) =>
      syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock)
    | CaseZR(_, block, zrules, Some(uty)) =>
      let ty = UHTyp.expand(uty);
      switch (Statics.syn_block(ctx, block)) {
      | None => None
      | Some(ty1) =>
        let zrule = GeneralUtil.ZList.prj_z(zrules);
        ana_cursor_info_rule(~node_steps, ~term_steps, ctx, zrule, ty1, ty);
      };
    | CaseZA(_, _, _, zann) =>
      cursor_info_typ(~node_steps, ~term_steps, ctx, zann)
    | ApPaletteZ(_, _, _, zpsi) =>
      let (ty, zblock) = GeneralUtil.ZNatMap.prj_z_v(zpsi.zsplice_map);
      ana_cursor_info_block(~node_steps, ~term_steps, ctx, zblock, ty);
    };
  }
  and ana_cursor_info_block =
      (
        ~node_steps,
        ~term_steps,
        ctx: Contexts.t,
        zblock: ZExp.zblock,
        ty: HTyp.t,
      )
      : option(t) =>
    switch (zblock) {
    | BlockZL((prefix, zline, suffix), conclusion) =>
      switch (Statics.syn_lines(ctx, prefix)) {
      | None => None
      | Some(ctx) =>
        syn_cursor_info_line(
          ~node_steps,
          ~term_steps,
          ~frame=(prefix, None, Some(Block(suffix, conclusion))),
          ~line_no=List.length(prefix),
          ctx,
          zline,
        )
      }
    | BlockZE(lines, ze) =>
      switch (Statics.syn_lines(ctx, lines)) {
      | None => None
      | Some(ctx) =>
        ana_cursor_info(
          ~node_steps,
          ~term_steps,
          ~frame=(lines, None, None),
          ctx,
          ze,
          ty,
        )
      }
    }
  and ana_cursor_info = (ctx: Contexts.t, ze: ZExp.t, ty: HTyp.t): option(t) =>
    switch (ze) {
    | CursorE(cursor, e) => ana_cursor_found_exp(ctx, e, ty, cursor)
    /* zipper cases */
    | ParenthesizedZ(zblock) =>
      ana_cursor_info_block(~node_steps, ~term_steps, ctx, zblock, ty)
    | OpSeqZ(skel, ze0, surround) =>
      let e0 = ZExp.erase_zoperand(ze0);
      let seq = Seq.t_of_operand_and_surround(e0, surround);
      let n = Seq.surround_prefix_length(surround);
      let (prefix, _, suffix) = frame;
      ana_cursor_info_skel(
        ~node_steps,
        ~term_steps,
        ~frame=(prefix, Some(surround), suffix),
        ctx,
        skel,
        seq,
        n,
        ze0,
        ty,
      );
    /* zipper in hole */
    | LamZP(InHole(WrongLength, _), _, _, _)
    | LamZA(InHole(WrongLength, _), _, _, _)
    | LamZE(InHole(WrongLength, _), _, _, _)
    | InjZ(InHole(WrongLength, _), _, _)
    | CaseZE(InHole(WrongLength, _), _, _, _)
    | CaseZR(InHole(WrongLength, _), _, _, _)
    | CaseZA(InHole(WrongLength, _), _, _, _)
    | ApPaletteZ(InHole(WrongLength, _), _, _, _) => None
    | LamZP(InHole(TypeInconsistent, _), _, _, _)
    | LamZA(InHole(TypeInconsistent, _), _, _, _)
    | LamZE(InHole(TypeInconsistent, _), _, _, _)
    | InjZ(InHole(TypeInconsistent, _), _, _)
    | CaseZE(InHole(TypeInconsistent, _), _, _, _)
    | CaseZR(InHole(TypeInconsistent, _), _, _, _)
    | CaseZA(InHole(TypeInconsistent, _), _, _, _)
    | ApPaletteZ(InHole(TypeInconsistent, _), _, _, _) =>
      syn_cursor_info(ctx, ze)
    /* zipper not in hole */
    | LamZP(NotInHole, zp, ann, _) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => None
      | Some((ty1_given, _)) =>
        let ty1 =
          switch (ann) {
          | Some(uty1) => UHTyp.expand(uty1)
          | None => ty1_given
          };
        ana_cursor_info(~node_steps, ~term_steps, ctx, zp, ty1);
      }
    | LamZA(NotInHole, _, zann, _) =>
      cursor_info_typ(~node_steps, ~term_steps, ctx, zann)
    | LamZE(NotInHole, p, ann, zblock) =>
      switch (HTyp.matched_arrow(ty)) {
      | None => None
      | Some((ty1_given, ty2)) =>
        let ty1 =
          switch (ann) {
          | Some(uty1) => UHTyp.expand(uty1)
          | None => ty1_given
          };
        switch (Statics.Pat.ana(ctx, p, ty1)) {
        | None => None
        | Some(ctx) =>
          ana_cursor_info_block(~node_steps, ~term_steps, ctx, zblock, ty2)
        };
      }
    | InjZ(NotInHole, position, zblock) =>
      switch (HTyp.matched_sum(ty)) {
      | None => None
      | Some((ty1, ty2)) =>
        ana_cursor_info_block(
          ~node_steps,
          ~term_steps,
          ctx,
          zblock,
          InjSide.pick(position, ty1, ty2),
        )
      }
    | CaseZE(NotInHole, zblock, _, _) =>
      syn_cursor_info_block(~node_steps, ~term_steps, ctx, zblock)
    | CaseZR(NotInHole, block, zrules, _) =>
      switch (Statics.syn_block(ctx, block)) {
      | None => None
      | Some(ty1) =>
        let zrule = ZList.prj_z(zrules);
        ana_cursor_info_rule(~node_steps, ~term_steps, ctx, zrule, ty1, ty);
      }
    | CaseZA(NotInHole, _, _, zann) =>
      cursor_info_typ(~node_steps, ~term_steps, ctx, zann)
    | ApPaletteZ(NotInHole, _, _, _) => syn_cursor_info(ctx, ze)
    }
  and ana_cursor_info_rule =
      (
        ~node_steps,
        ~term_steps,
        ctx: Contexts.t,
        zrule: ZExp.zrule,
        pat_ty: HTyp.t,
        clause_ty: HTyp.t,
      )
      : option(t) =>
    switch (zrule) {
    | CursorR(cursor, rule) =>
      Some(
        mk_cursor_info(
          OnRule,
          Rule(rule),
          ExpFrame([], None, None),
          cursor,
          ctx,
          node_steps,
          term_steps,
        ),
      )
    | RuleZP(zp, _) =>
      ana_cursor_info(~node_steps, ~term_steps, ctx, zp, pat_ty)
    | RuleZE(p, zblock) =>
      switch (Statics.Pat.ana(ctx, p, pat_ty)) {
      | None => None
      | Some(ctx) =>
        ana_cursor_info_block(
          ~node_steps,
          ~term_steps,
          ctx,
          zblock,
          clause_ty,
        )
      }
    }
  and syn_cursor_info_skel =
      (
        ctx: Contexts.t,
        skel: UHExp.skel,
        seq: UHExp.opseq,
        n: int,
        ze_n: ZExp.t,
      )
      : option(t) => {
    switch (skel) {
    | Placeholder(n') =>
      if (n == n') {
        syn_cursor_info(ctx, ze_n);
      } else {
        None;
      }
    | BinOp(_, Minus, skel1, skel2)
    | BinOp(_, Plus, skel1, skel2)
    | BinOp(_, Times, skel1, skel2)
    | BinOp(_, LessThan | GreaterThan | Equals, skel1, skel2) =>
      switch (ana_cursor_info_skel(ctx, skel1, seq, n, ze_n, Num)) {
      | Some(_) as result => result
      | None =>
        switch (ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, Num)) {
        | Some(_) as result => result
        | None => None
        }
      }
    | BinOp(_, And | Or, skel1, skel2) =>
      switch (ana_cursor_info_skel(ctx, skel1, seq, n, ze_n, Bool)) {
      | Some(_) as result => result
      | None =>
        switch (ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, Bool)) {
        | Some(_) as result => result
        | None => None
        }
      }
    | BinOp(_, Space, Placeholder(n') as skel1, skel2) =>
      if (n == n') {
        switch (syn_cursor_info(ctx, ze_n)) {
        | None => None
        | Some(ci) =>
          switch (ZExp.cursor_on_outer_expr(ze_n)) {
          | Some((
              Block(_, Var(InHole(TypeInconsistent, _), _, _)) as outer_block,
              _position,
            ))
          | Some((
              Block(_, NumLit(InHole(TypeInconsistent, _), _)) as outer_block,
              _position,
            ))
          | Some((
              Block(_, BoolLit(InHole(TypeInconsistent, _), _)) as outer_block,
              _position,
            ))
          | Some((
              Block(_, ListNil(InHole(TypeInconsistent, _))) as outer_block,
              _position,
            ))
          | Some((
              Block(_, Lam(InHole(TypeInconsistent, _), _, _, _)) as outer_block,
              _position,
            ))
          | Some((
              Block(_, Inj(InHole(TypeInconsistent, _), _, _)) as outer_block,
              _position,
            ))
          | Some((
              Block(_, Case(InHole(TypeInconsistent, _), _, _, _)) as outer_block,
              _position,
            ))
          | Some((
              Block(_, ApPalette(InHole(TypeInconsistent, _), _, _, _)) as outer_block,
              _position,
            )) =>
            let outer_block_nih =
              UHExp.set_err_status_block(NotInHole, outer_block);
            switch (Statics.syn_block(ctx, outer_block_nih)) {
            | None => None
            | Some(ty) =>
              Some({...ci, typed: SynErrorArrow(Arrow(Hole, Hole), ty)})
            };
          | Some((
              Block(_, Var(_, InVarHole(Keyword(k), _), _)),
              _position,
            )) =>
            Some({...ci, typed: SynKeywordArrow(Arrow(Hole, Hole), k)})
          | Some((Block(_, Var(_, InVarHole(Free, _), _)), _position)) =>
            Some({...ci, typed: SynFreeArrow(Arrow(Hole, Hole))})
          | Some((outer_block, _position)) =>
            switch (Statics.syn_block(ctx, outer_block)) {
            | None => None
            | Some(ty) =>
              switch (HTyp.matched_arrow(ty)) {
              | None => None
              | Some((ty1, ty2)) =>
                Some({...ci, typed: SynMatchingArrow(ty, Arrow(ty1, ty2))})
              }
            }
          | None => syn_cursor_info(ctx, ze_n)
          }
        };
      } else {
        switch (Statics.syn_skel(ctx, skel1, seq)) {
        | None => None
        | Some((ty, _)) =>
          switch (HTyp.matched_arrow(ty)) {
          | None => None
          | Some((ty1, _)) =>
            ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty1)
          }
        };
      }
    | BinOp(_, Space, skel1, skel2) =>
      switch (syn_cursor_info_skel(ctx, skel1, seq, n, ze_n)) {
      | Some(_) as result => result
      | None =>
        switch (Statics.syn_skel(ctx, skel1, seq)) {
        | None => None
        | Some((ty, _)) =>
          switch (HTyp.matched_arrow(ty)) {
          | None => None
          | Some((ty1, _)) =>
            ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty1)
          }
        }
      }
    | BinOp(_, Comma, skel1, skel2) =>
      switch (syn_cursor_info_skel(ctx, skel1, seq, n, ze_n)) {
      | Some(_) as result => result
      | None => syn_cursor_info_skel(ctx, skel2, seq, n, ze_n)
      }
    | BinOp(_, Cons, skel1, skel2) =>
      switch (syn_cursor_info_skel(ctx, skel1, seq, n, ze_n)) {
      | Some(_) as result => result
      | None =>
        switch (Statics.syn_skel(ctx, skel1, seq)) {
        | None => None
        | Some((ty_elt, _)) =>
          let ty_list = HTyp.List(ty_elt);
          ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty_list);
        }
      }
    };
  }
  and ana_cursor_info_skel =
      (
        ctx: Contexts.t,
        skel: UHExp.skel,
        seq: UHExp.opseq,
        n: int,
        ze_n: ZExp.t,
        ty: HTyp.t,
      )
      : option(t) =>
    switch (skel) {
    | Placeholder(n') =>
      if (n == n') {
        ana_cursor_info(ctx, ze_n, ty);
      } else {
        None;
      }
    | BinOp(InHole(TypeInconsistent, _), _, _, _) =>
      syn_cursor_info_skel(ctx, skel, seq, n, ze_n)
    | BinOp(NotInHole, Comma, skel1, skel2) =>
      switch (ty) {
      | Hole =>
        switch (ana_cursor_info_skel(ctx, skel1, seq, n, ze_n, Hole)) {
        | Some(_) as result => result
        | None => ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, Hole)
        }
      | Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHExp.get_tuple(skel1, skel2);
        switch (ListMinTwo.zip_eq(skels, types)) {
        | None => None
        | Some(zipped) =>
          List.fold_left(
            (opt_result, skel_ty: (UHExp.skel, HTyp.t)) =>
              switch (opt_result) {
              | Some(_) as result => result
              | None =>
                let (skel, ty) = skel_ty;
                ana_cursor_info_skel(ctx, skel, seq, n, ze_n, ty);
              },
            None,
            ListMinTwo.to_list(zipped),
          )
        };
      | _ => None
      }
    | BinOp(InHole(WrongLength, _), Comma, skel1, skel2) =>
      switch (ty) {
      | Prod(ty1, ty2) =>
        let types = HTyp.get_tuple(ty1, ty2);
        let skels = UHExp.get_tuple(skel1, skel2);
        let (zipped, remainder) = HTyp.zip_with_skels(skels, types);
        let ana_zipped =
          List.fold_left(
            (opt_result, skel_ty: (UHExp.skel, HTyp.t)) =>
              switch (opt_result) {
              | Some(_) as result => result
              | None =>
                let (skel, ty) = skel_ty;
                ana_cursor_info_skel(ctx, skel, seq, n, ze_n, ty);
              },
            None,
            ListMinTwo.to_list(zipped),
          );
        switch (ana_zipped) {
        | Some(_) as result => result
        | None =>
          List.fold_left(
            (opt_result, skel) =>
              switch (opt_result) {
              | Some(_) as result => result
              | None => syn_cursor_info_skel(ctx, skel, seq, n, ze_n)
              },
            None,
            remainder,
          )
        };
      | _ => None
      }
    | BinOp(InHole(WrongLength, _), _, _, _) => None
    | BinOp(NotInHole, Cons, skel1, skel2) =>
      switch (HTyp.matched_list(ty)) {
      | None => None
      | Some(ty_elt) =>
        switch (ana_cursor_info_skel(ctx, skel1, seq, n, ze_n, ty_elt)) {
        | Some(_) as result => result
        | None =>
          let ty_list = HTyp.List(ty_elt);
          ana_cursor_info_skel(ctx, skel2, seq, n, ze_n, ty_list);
        }
      }
    | BinOp(_, Minus | And | Or, _, _)
    | BinOp(_, Plus, _, _)
    | BinOp(_, Times, _, _)
    | BinOp(_, LessThan | GreaterThan | Equals, _, _)
    | BinOp(_, Space, _, _) => syn_cursor_info_skel(ctx, skel, seq, n, ze_n)
    };
};
