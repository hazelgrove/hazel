open SemanticsCommon;

let mk_OpSeq = (seq: UHTyp.opseq): UHTyp.t_inner => {
  let skel = Associator.associate_ty(seq);
  OpSeq(skel, seq);
};

let mk_OpSeqZ = (zty: ZTyp.t, surround: ZTyp.opseq_surround): ZTyp.t => {
  let skel =
    Associator.associate_ty(
      OperatorSeq.opseq_of_exp_and_surround(ZTyp.erase(zty), surround),
    );
  OpSeqZ(skel, zty, surround);
};

let opseq_of_prefix_and_uty =
    (prefix: ZTyp.opseq_prefix, uty: UHTyp.t): UHTyp.opseq =>
  switch (uty) {
  | TI(OpSeq(_, seq)) => OperatorSeq.opseq_of_prefix_and_seq(prefix, seq)
  | _ => OperatorSeq.opseq_of_prefix_and_exp(prefix, uty)
  };

[@deriving sexp]
type frame =
  | ParenthesizedF
  | ListF
  | OpSeqF(ZTyp.opseq_surround);

[@deriving sexp]
type context =
  | Slot
  | Frame(frame, context);

let opseq_of_zty_and_surround =
    (zty: ZTyp.t, surround: ZTyp.opseq_surround): ZTyp.t =>
  switch (zty) {
  | OpSeqZ(_, zty, inner_surround) =>
    let surround = OperatorSeq.nest_surrounds(inner_surround, surround);
    mk_OpSeqZ(zty, surround);
  | CursorTI(BeforeChild(k, side), OpSeq(_, seq)) =>
    let prefix_len = OperatorSeq.surround_prefix_length(surround);
    let seq = OperatorSeq.opseq_of_seq_and_surround(seq, surround);
    CursorTI(BeforeChild(prefix_len + k, side), mk_OpSeq(seq));
  | CursorTI(_, _)
  | CursorTO(_, _)
  | ParenthesizedZ(_)
  | ListZ(_) => mk_OpSeqZ(zty, surround)
  };

let rec fill_context = (zty: ZTyp.t, ctx: context): ZTyp.t =>
  switch (ctx) {
  | Slot => zty
  | Frame(ParenthesizedF, ctx) => ParenthesizedZ(fill_context(zty, ctx))
  | Frame(ListF, ctx) => ListZ(fill_context(zty, ctx))
  | Frame(OpSeqF(surround), ctx) =>
    opseq_of_zty_and_surround(fill_context(zty, ctx), surround)
  };

let rec pop_frame = (ctx: context): option((frame, context)) =>
  switch (ctx) {
  | Slot => None
  | Frame(frame1, ctx1) =>
    switch (pop_frame(ctx1)) {
    | None => Some((frame1, ctx1))
    | Some((frame2, ctx2)) => Some((frame2, Frame(frame1, ctx2)))
    }
  };

let rec push_frame = (frame: frame, ctx: context): context =>
  switch (ctx) {
  | Slot => Frame(frame, Slot)
  | Frame(frame1, ctx1) => Frame(frame1, push_frame(frame, ctx1))
  };

let rec split_over_cursor_on_parens =
        (zty: ZTyp.t): option((inner_cursor, UHTyp.t, context)) =>
  switch (zty) {
  | CursorTO(_, _)
  | CursorTI(_, List(_) | OpSeq(_, _)) => None
  | CursorTI(inner_cursor, Parenthesized(uty)) =>
    Some((inner_cursor, uty, Slot))
  | ParenthesizedZ(zty) =>
    switch (split_over_cursor_on_parens(zty)) {
    | None => None
    | Some((inner_cursor, uty, uty_ctx)) =>
      Some((inner_cursor, uty, Frame(ParenthesizedF, uty_ctx)))
    }
  | ListZ(zty) =>
    switch (split_over_cursor_on_parens(zty)) {
    | None => None
    | Some((inner_cursor, uty, uty_ctx)) =>
      Some((inner_cursor, uty, Frame(ListF, uty_ctx)))
    }
  | OpSeqZ(_, zty, surround) =>
    switch (split_over_cursor_on_parens(zty)) {
    | None => None
    | Some((inner_cursor, uty, uty_ctx)) =>
      Some((inner_cursor, uty, Frame(OpSeqF(surround), uty_ctx)))
    }
  };
