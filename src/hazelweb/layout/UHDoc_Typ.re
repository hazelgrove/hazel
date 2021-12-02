open UHDoc_common;
// module Doc = UHDoc_common.Doc;

let inline_padding_of_operator: UHTyp.operator => (UHDoc.t, UHDoc.t) =
  fun
  | Prod => (empty_, space_)
  | Arrow => (space_, space_);

let inline_padding_of_sum_body_operator:
  UHTyp.sum_body_operator => (UHDoc.t, UHDoc.t) =
  fun
  | Plus => (space_, space_);

let mk_EmptyHole: string => UHDoc.t = mk_EmptyHole(~sort=Typ);
let mk_Parenthesized: formatted_child => UHDoc.t =
  mk_Parenthesized(~sort=Typ);
let mk_NTuple:
  (
    ~mk_operand: (~enforce_inline: bool, 'a) => UHDoc.t,
    ~mk_operator: UHTyp.operator => UHDoc.t,
    ~enforce_inline: bool,
    OpSeq.t('a, UHTyp.operator)
  ) =>
  UHDoc.t =
  UHDoc_common.mk_NTuple(
    ~sort=Typ,
    ~get_tuple_elements=UHTyp.get_prod_elements,
    ~inline_padding_of_operator,
  );

let mk_ConstTag = (tag: formatted_child): UHDoc.t =>
  tag |> pad_delimited_closed_child(~sort=Tag);

let mk_ArgTag = (tag: formatted_child, body: formatted_child): UHDoc.t =>
  Doc.hcats([
    Delim.mk(~index=-1, ""),
    tag |> pad_delimited_closed_child(~sort=Tag),
    Delim.open_Parenthesized(),
    body |> pad_delimited_closed_child(~sort=Typ),
    Delim.close_Parenthesized(),
  ])
  |> annot_Tessera
  |> annot_Operand(~sort=SumBody);

let mk_Tag =
    (tag: formatted_child, arg_opt: option(formatted_child)): UHDoc.t =>
  switch (arg_opt) {
  | None => mk_ConstTag(tag)
  | Some(arg) => mk_ArgTag(tag, arg)
  };

let mk_Sum =
    (
      sum_body_opt: option(formatted_child),
      open_delim: UHDoc.t,
      close_delim: UHDoc.t,
    )
    : UHDoc.t => {
  let open_group = open_delim |> annot_Tessera;
  let close_group = close_delim |> annot_Tessera;
  let sum_body =
    switch (sum_body_opt) {
    | None => Doc.empty() |> pad_closed_inline_child((empty_, empty_), Typ)
    | Some(sum_body) => sum_body |> pad_delimited_closed_child(~sort=Typ)
    };
  Doc.hcats([open_group, sum_body, close_group])
  |> annot_Tessera
  |> annot_Operand(~sort=Typ);
};

let mk_FiniteSum = (sum_body_opt: option(formatted_child)): UHDoc.t =>
  mk_Sum(sum_body_opt, Delim.open_FiniteSum(), Delim.close_FiniteSum());

let mk_ElidedSum = (operand: UHDoc.t): UHDoc.t => {
  let sum_body = EnforcedInline(operand |> annot_Step(0));
  mk_Sum(Some(sum_body), Delim.open_ElidedSum(), Delim.close_ElidedSum());
};

let mk_SumBody =
    (
      ~mk_operand: (~enforce_inline: bool, 'operand) => UHDoc.t,
      ~mk_operator: UHTyp.sum_body_operator => UHDoc.t,
      ~enforce_inline: bool,
      OpSeq(skel, seq): OpSeq.t('operand, UHTyp.sum_body_operator),
    )
    : UHDoc.t => {
  let mk_BinOp =
    UHDoc_common.mk_BinOp(
      ~sort=SumBody,
      ~mk_operand,
      ~mk_operator,
      ~inline_padding_of_operator=inline_padding_of_sum_body_operator,
      ~seq,
    );
  switch (UHTyp.get_sum_body_elements(skel)) {
  /* Empty sums are handled by mk_Sum */
  | [] => failwith(__LOC__ ++ ": found empty sum body")
  | [singleton] => mk_BinOp(~enforce_inline, singleton)
  | [hd, ...tl] =>
    let hd_doc = (~enforce_inline: bool) => {
      let format =
        UHAnnot.OpenChild(enforce_inline ? InlineWithBorder : Multiline);
      hd |> mk_BinOp(~enforce_inline) |> Doc.annot(format);
    };
    let plus_doc = (step: int) => annot_Step(step, mk_op("+"));
    let (inline_choice, plus_indices) =
      tl
      |> List.fold_left(
           ((sum_body, plus_indices), elem) => {
             let plus_index =
               Skel.leftmost_tm_index(elem) - 1 + Seq.length(seq);
             let elem_doc = mk_BinOp(~enforce_inline=true, elem);
             let doc =
               Doc.(
                 hcats([
                   sum_body,
                   space_ |> annot(UHAnnot.OpenChild(InlineWithBorder)),
                   plus_doc(plus_index) |> annot_Tessera,
                   hcats([space_, elem_doc])
                   |> annot(UHAnnot.OpenChild(InlineWithBorder)),
                 ])
               );
             (doc, [plus_index, ...plus_indices]);
           },
           (hd_doc(~enforce_inline=true), []),
         );
    let multiline_choice =
      tl
      |> List.fold_left(
           (sum_body, elem) => {
             let plus_index =
               Skel.leftmost_tm_index(elem) - 1 + Seq.length(seq);
             let elem_doc = mk_BinOp(~enforce_inline=false, elem);
             Doc.(
               vsep(
                 sum_body,
                 hcat(
                   plus_doc(plus_index) |> annot_Tessera,
                   // TODO need to have a choice here for multiline vs not
                   hcat(space_, align(elem_doc))
                   |> annot(UHAnnot.OpenChild(Multiline)),
                 ),
               )
             );
           },
           hd_doc(~enforce_inline=false),
         );
    let choices =
      enforce_inline
        ? inline_choice : Doc.choice(inline_choice, multiline_choice);
    choices
    |> Doc.annot(
         UHAnnot.mk_Term(
           ~sort=SumBody,
           ~shape=SumBody({plus_indices: plus_indices}),
           (),
         ),
       );
  };
};

let rec mk =
  lazy(
    memoize((~memoize: bool, ~enforce_inline: bool, uty: UHTyp.t) =>
      (Lazy.force(mk_opseq, ~memoize, ~enforce_inline, uty): UHDoc.t)
    )
  )
and mk_opseq =
  lazy(
    memoize((~memoize: bool, ~enforce_inline: bool, opseq: UHTyp.opseq) =>
      (
        mk_NTuple(
          ~mk_operand=Lazy.force(mk_operand, ~memoize),
          ~mk_operator,
          ~enforce_inline,
          opseq,
        ): UHDoc.t
      )
    )
  )
and mk_operator = (op: UHTyp.operator): UHDoc.t =>
  mk_op(Operators_Typ.to_string(op))
and mk_operand =
  lazy(
    memoize((~memoize: bool, ~enforce_inline: bool, operand: UHTyp.operand) =>
      (
        switch (operand) {
        | Hole => mk_EmptyHole("?")
        | Unit => mk_Unit()
        | Int => mk_Int()
        | Float => mk_Float()
        | Bool => mk_Bool()
        | Parenthesized(body) =>
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=0, body);
          mk_Parenthesized(body);
        | List(body) =>
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=0, body);
          mk_List(body);
        | FiniteSum(None) => mk_FiniteSum(None)
        | FiniteSum(Some(sum_body)) =>
          let sum_body =
            mk_sum_body(~memoize, ~enforce_inline, ~child_step=0, sum_body);
          mk_FiniteSum(Some(sum_body));
        | ElidedSum(operand) =>
          let operand_doc =
            Lazy.force(
              mk_sum_body_operand,
              ~memoize,
              ~enforce_inline,
              operand,
            );
          mk_ElidedSum(operand_doc);
        }: UHDoc.t
      )
    )
  )
and mk_child =
    (~memoize: bool, ~enforce_inline: bool, ~child_step: int, uty: UHTyp.t)
    : formatted_child => {
  let formattable = (~enforce_inline: bool) =>
    Lazy.force(mk, ~memoize, ~enforce_inline, uty) |> annot_Step(child_step);
  enforce_inline
    ? EnforcedInline(formattable(~enforce_inline=true))
    : Unformatted(formattable);
}
and mk_sum_body =
    (
      ~memoize: bool,
      ~enforce_inline: bool,
      ~child_step: int,
      sum_body: UHTyp.sum_body,
    )
    : formatted_child => {
  let formattable = (~enforce_inline: bool) =>
    Lazy.force(mk_sum_body_opseq, ~memoize, ~enforce_inline, sum_body)
    |> annot_Step(child_step);
  enforce_inline
    ? EnforcedInline(formattable(~enforce_inline))
    : Unformatted(formattable);
}
and mk_sum_body_opseq =
  lazy(
    memoize((~memoize: bool, ~enforce_inline: bool, opseq: UHTyp.sum_body) =>
      (
        mk_SumBody(
          ~mk_operand=Lazy.force(mk_sum_body_operand, ~memoize),
          ~mk_operator=mk_sum_body_operator,
          ~enforce_inline,
          opseq,
        ): UHDoc.t
      )
    )
  )
and mk_sum_body_operator = (op: UHTyp.sum_body_operator): UHDoc.t =>
  mk_op(Operators_SumBody.to_string(op))
and mk_sum_body_operand =
  lazy(
    memoize(
      (~memoize: bool, ~enforce_inline: bool, operand: UHTyp.sum_body_operand) =>
      (
        switch (operand) {
        | ConstTag(tag) =>
          let tag_doc =
            UHDoc_Tag.mk_formatted(~memoize, ~enforce_inline, tag);
          mk_ConstTag(tag_doc);
        | ArgTag(tag, ty) =>
          let tag_doc =
            UHDoc_Tag.mk_child(~memoize, ~enforce_inline, ~child_step=0, tag);
          let body = mk_child(~memoize, ~enforce_inline, ~child_step=1, ty);
          mk_ArgTag(tag_doc, body);
        }: UHDoc.t
      )
    )
  );
