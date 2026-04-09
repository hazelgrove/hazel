open StaticsBase;

type tuple_entry =
  | Unlabeled(Typ.t)
  | Labeled(option(string), Typ.t);
type tuple_type = list(tuple_entry);

// Constants and helper functions
let unknown = Unknown(Internal) |> Typ.temp;
let syn = Unknown(SynSwitch) |> Typ.temp;

let rebuild_exp_info_local =
    (
      ~uexp: Exp.t,
      ~ctx,
      ~ana,
      ~ancestors,
      ~syn_ty: Typ.t,
      ~co_ctx,
      ~label_inference: option(Info.label_inference(Info.exp)),
      ~inferred_label: option(LabeledTuple.label),
      ~dot_labels: list(string),
      ~label_sort,
      ~marks: list(Mark.t),
      ~warnings: list(Warning.list_item),
    ): Info.exp => {
  let marks =
    switch (uexp.term) {
    | Deferral(InAp) => marks
    | _ when marks != [] => marks
    | _ =>
      switch (expectation_mismatch_mark(ctx, ana, syn_ty)) {
      | None => marks
      | Some(m) => marks @ [m]
      }
    };
  let message =
    marks != []
      ? Message.Exp(Message.Default)
      : Message.Exp(
          switch (uexp.term) {
          | Deferral(InAp) => Message.AnaDeferralConsistent(ana)
          | _ =>
            switch (ana) {
            | {term: Unknown(SynSwitch), _} => Message.Default
            | _ => Message.Common(syn_ana_ok_common(ctx, ana, syn_ty))
            }
          },
        );
  let cls = Cls.Exp(Exp.cls_of_term(uexp.term));
  let ty = fixed_typ(ctx, ana, syn_ty);
  {
    cls,
    syn_ty,
    marks,
    ty,
    ana,
    message,
    warnings,
    ctx,
    co_ctx,
    ancestors,
    user_term: uexp,
    label_inference,
    inferred_label,
    label_sort,
    dot_labels,
  };
};

let rewrite_exp_info =
    (
      m: Map.t,
      info: Info.exp,
      ~syn_ty: Typ.t=info.syn_ty,
      ~marks: list(Mark.t)=info.marks,
      ~label_sort: bool=info.label_sort,
      ~dot_labels: list(string)=info.dot_labels,
      (),
    ): (Info.exp, Map.t) => {
  let updated =
    rebuild_exp_info_local(
      ~uexp=info.user_term,
      ~ctx=info.ctx,
      ~ana=info.ana,
      ~ancestors=info.ancestors,
      ~syn_ty,
      ~marks,
      ~co_ctx=info.co_ctx,
      ~label_inference=info.label_inference,
      ~inferred_label=info.inferred_label,
      ~dot_labels,
      ~label_sort,
      ~warnings=info.warnings,
    );
  (
    updated,
    Map.add_info(IdTagged.ids(info.user_term), InfoExp(updated), m),
  );
};

let append_marks_for_term = (m: Map.t, e: Exp.t, extra: list(Mark.t)): Map.t =>
  switch (Map.lookup(Exp.rep_id(e), m)) {
  | Some(Info.InfoExp(info)) =>
    rewrite_exp_info(m, info, ~marks=info.marks @ extra, ()) |> snd
  | _ => m
  };

let typ_entry_to_tuple_entry = (entry: Typ.t) => {
  switch (entry.term) {
  | TupLabel({term: Label(l), _}, typ) => Labeled(Some(l), typ)
  | TupLabel(_, typ) => Labeled(None, typ)
  | _ => Unlabeled(entry)
  };
};

let extract_label = (entry: tuple_entry): option(string) =>
  switch (entry) {
  | Labeled(Some(label), _) => Some(label)
  | _ => None
  };

let extract_type = (entry: tuple_entry): Typ.t =>
  switch (entry) {
  | Unlabeled(typ) => typ
  | Labeled(_, typ) => typ
  };

let extract_labels = (entries: tuple_type) =>
  List.filter_map((entry: tuple_entry) => extract_label(entry), entries);

let get_tuple_label = (tuple: tuple_type, label: string): Typ.t => {
  switch (
    List.find_opt(entry => extract_label(entry) == Some(label), tuple)
  ) {
  | Some(entry) => extract_type(entry)
  | None => unknown
  };
};

type extract_result =
  | Success(list(Typ.t))
  | Unknown // Type is unknown
  | Failure; // Type is wrong

// Generic argument analysis
let analyze_argument =
    (
      module S: ExpressionStatics,
      ~ctx,
      ~error_override: Mark.t,
      ~extract_entries,
      m,
      arg,
    ) => {
  open S;
  let (arg_info, _, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);

  switch (extract_entries(Typ.normalize(ctx, arg_info.ty))) {
  | Success(entries) => (
      Some(List.map(typ_entry_to_tuple_entry, entries)),
      arg_info,
      m,
    )
  | Unknown => (None, arg_info, m)
  | Failure =>
    let m =
      rewrite_exp_info(m, arg_info, ~marks=arg_info.marks @ [error_override], ())
      |> snd;
    (None, arg_info, m);
  };
};

let analyze_tuple_argument = (module S: ExpressionStatics, ~ctx, m, tup) => {
  analyze_argument(
    (module S),
    ~ctx,
    ~error_override=BuiltinError(ArgumentMustBeTuple),
    ~extract_entries=
      typ =>
        switch (typ.term) {
        | Prod(entries) => Success(entries)
        | Unknown(_) => Unknown
        | _ => Failure
        },
    m,
    tup,
  );
};

let analyze_table_argument = (module S: ExpressionStatics, ~ctx, m, table) => {
  analyze_argument(
    (module S),
    ~ctx,
    ~error_override=BuiltinError(ArgumentMustBeListOfTuples),
    ~extract_entries=
      typ =>
        switch (typ.term) {
        | List({term: Prod(entries), _}) => Success(entries)
        | List({term: Unknown(_), _}) => Unknown
        | Unknown(_) => Unknown
        | _ => Failure
        },
    m,
    table,
  );
};

/* Analyze an expression in label position and mark label-sort metadata. */
let analyze_label_to_info_map =
    (
      module S: ExpressionStatics,
      ~ctx,
      labmode: Typ.t,
      label: Exp.t,
      m: Map.t,
    ) => {
  let lab_name =
    switch (label.term) {
    | Label(lab) => Some(lab)
    | EmptyHole
    | _ => None
    };
  let (i, i_elab, m) = S.uexp_to_info_map(~ctx, ~ana=labmode, label, m);
  let (i_unpatched, syn_ty, marks) =
    switch (label.term) {
    | Label(name) => (i, Label(name) |> Typ.temp, [])
    | EmptyHole => (i, i.syn_ty, i.marks)
    | _ => {
        let i_unpatched = {
          ...i,
          marks: i.marks @ [BadLabel(Exp(label))],
        };
        (i_unpatched, i_unpatched.syn_ty, i_unpatched.marks);
      }
    };
  let (_, m) =
    rewrite_exp_info(m, i_unpatched, ~syn_ty, ~marks, ~label_sort=true, ());
  (
    lab_name,
    {
      ...i_unpatched,
      label_sort: true,
    },
    i_elab,
    m,
  );
};

let labels_to_info_map =
    (
      module S: ExpressionStatics,
      ~ctx: Ctx.t,
      ~expected_labels: option(list(string))=?,
      labs: list(Exp.t),
      m: Map.t,
    )
    : (list(option(string)), Map.t) => {
  List.fold_left(
    ((labels: list(option(string)), m: Map.t), label) => {
      let (lab_name, lab_info, _, m) =
        analyze_label_to_info_map((module S), ~ctx, syn, label, m);
      /* If expected_labels provided and this label isn't in the set,
         patch as InvalidLabel and suppress the label name */
      let (lab_name, m) =
        switch (label.term, expected_labels, lab_name) {
        | (Label(name), Some(expected), _) when !List.mem(name, expected) =>
          let (_, m) =
            rewrite_exp_info(
              m,
              lab_info,
              ~marks=lab_info.marks @ [InvalidLabel(name, expected)],
              (),
            );
          (None, m);
        | _ => (lab_name, m)
        };
      (labels @ [lab_name], m);
    },
    ([], m),
    labs,
  );
};

// Common fallback for invalid arguments
let invalid_args_fallback =
    (module S: ExpressionStatics, ~ctx, ~fn_info: Info.exp, ~error, m, arg) => {
  S.(
    let (arg_info, _, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);
    add(
      ~syn_ty=unknown,
      ~marks=[error],
      ~co_ctx=CoCtx.union([fn_info.co_ctx, arg_info.co_ctx]),
      m,
    );
  );
};

// Generic tuple operation handler
let handle_tuple_operation =
    (
      module S: ExpressionStatics,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      ~compute_result_type,
      m: Map.t,
      arg: Exp.t,
    ) => {
  S.(
    switch (arg.term) {
    | Tuple([tup, ...labs]) when List.length(labs) > 0 =>
      let (labeled_tup_info: option(tuple_type), tup_info, m: Map.t) =
        analyze_tuple_argument((module S), ~ctx, m, tup);

      let expected_labels = Option.map(extract_labels, labeled_tup_info);
      let (labels, m) =
        labels_to_info_map((module S), ~ctx, ~expected_labels?, labs, m);

      let args_typ =
        Typ.to_product([tup_info.ty] @ List.map(__ => unknown, labs));

      let m =
        Map.add_info(
          arg.annotation.ids,
          InfoExp({
            cls: Cls.Exp(Exp.cls_of_term(arg.term)),
            syn_ty: args_typ,
            marks: [],
            ty: fixed_typ(ctx, syn, args_typ),
            ana: syn,
            message: Message.Exp(Message.Default),
            warnings: [],
            ctx,
            co_ctx: CoCtx.empty,
            ancestors,
            user_term: arg,
            label_inference: None,
            inferred_label: None,
            label_sort: false,
            dot_labels: [],
          }),
          m,
        );

      let result_type = compute_result_type(labeled_tup_info, labels);
      add(
        ~syn_ty=result_type,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
        m,
      );
    | _ =>
      invalid_args_fallback(
        (module S),
        ~ctx,
        ~fn_info,
        ~error=BuiltinError(AtLeast2Arguments),
        m,
        arg,
      )
    }
  );
};

let project_labels_statics =
    (
      module S: ExpressionStatics,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  handle_tuple_operation(
    (module S),
    ~fn_info,
    ~ancestors,
    ~ctx,
    ~compute_result_type=
      (labeled_tup_info, labels) => {
        let val_types =
          List.map(
            (optional_lab: option(string)) => {
              Util.OptUtil.map2(
                get_tuple_label,
                labeled_tup_info,
                optional_lab,
              )
              |> Option.value(~default=unknown)
            },
            labels,
          );
        Typ.to_product(val_types);
      },
    m,
    arg,
  );
};

let select_labels_statics =
    (
      module S: ExpressionStatics,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  handle_tuple_operation(
    (module S),
    ~fn_info,
    ~ancestors,
    ~ctx,
    ~compute_result_type=
      (labeled_tup_info, labels) => {
        let val_types =
          List.map(
            (optional_lab: option(string)) => {
              Util.OptUtil.map2(
                (a, b) =>
                  TupLabel(Label(b) |> Typ.temp, get_tuple_label(a, b))
                  |> Typ.temp,
                labeled_tup_info,
                optional_lab,
              )
              |> Option.value(~default=unknown)
            },
            labels,
          );
        Typ.to_product(val_types);
      },
    m,
    arg,
  );
};

let omit_labels_statics =
    (
      module S: ExpressionStatics,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  handle_tuple_operation(
    (module S),
    ~fn_info,
    ~ancestors,
    ~ctx,
    ~compute_result_type=
      (labeled_tup_info, labels) => {
        let labels_to_drop = List.filter_map(Fun.id, labels);
        switch (labeled_tup_info) {
        | None => unknown
        | Some(labeled_tup_info) =>
          let tys =
            List.filter_map(
              entry => {
                switch (entry) {
                | Unlabeled(typ) => Some(typ)
                | Labeled(None, typ) =>
                  Some(TupLabel(unknown, typ) |> Typ.temp)
                | Labeled(Some(lab), typ) =>
                  if (List.mem(lab, labels_to_drop)) {
                    None;
                  } else {
                    Some(TupLabel(Label(lab) |> Typ.temp, typ) |> Typ.temp);
                  }
                }
              },
              labeled_tup_info,
            );
          Typ.to_product(tys);
        };
      },
    m,
    arg,
  );
};

let group_by_label_statics =
    (
      module S: ExpressionStatics,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  S.(
    switch (arg.term) {
    | Tuple([table, pivot_label]) =>
      let (row_info: option(tuple_type), table_info, m) =
        analyze_table_argument((module S), ~ctx, m, table);

      let expected_labels = Option.map(extract_labels, row_info);
      let (label, _, _, m) =
        analyze_label_to_info_map((module S), ~ctx, syn, pivot_label, m);
      /* Patch InvalidLabel if not in expected set */
      let m =
        switch (pivot_label.term, expected_labels) {
        | (Label(name), Some(expected)) when !List.mem(name, expected) =>
          switch (Id.Map.find_opt(Exp.rep_id(pivot_label), m)) {
          | Some(Info.InfoExp(lab_info)) =>
            rewrite_exp_info(
              m,
              lab_info,
              ~marks=lab_info.marks @ [InvalidLabel(name, expected)],
              (),
            )
            |> snd
          | _ => m
          }
        | _ => m
        };

      let m =
        Map.add_info(
          arg.annotation.ids,
          InfoExp({
            cls: Cls.Exp(Exp.cls_of_term(arg.term)),
            syn_ty: Prod([table_info.ty, unknown]) |> Typ.temp,
            marks: [],
            ty: fixed_typ(ctx, syn, Prod([table_info.ty, unknown]) |> Typ.temp),
            ana: syn,
            message: Message.Exp(Message.Default),
            warnings: [],
            ctx,
            co_ctx: CoCtx.empty,
            ancestors,
            user_term: arg,
            label_inference: None,
            inferred_label: None,
            label_sort: false,
            dot_labels: [],
          }),
          m,
        );

      let pivot_type =
        Util.OptUtil.map2(
          (entries: list(tuple_entry), label: string) => {
            List.find_map(
              entry =>
                extract_label(entry) == Some(label)
                  ? Some(extract_type(entry)) : None,
              entries,
            )
          },
          row_info,
          label,
        )
        |> Option.join;

      let m =
        switch (pivot_type) {
        | Some(ty) when !Typ.is_consistent(ctx, ty, Typ.temp(Atom(String))) =>
          append_marks_for_term(
            m,
            pivot_label,
            [BuiltinError(PivotLabelIsNotString(ty))],
          )
        | _ => m
        };

      add(
        ~syn_ty=unknown,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, table_info.co_ctx]),
        m,
      );
    | _ =>
      invalid_args_fallback(
        (module S),
        ~ctx,
        ~fn_info,
        ~error=BuiltinError(Exactly2Arguments),
        m,
        arg,
      )
    }
  );
};

let to_lvs_statics =
    (
      module S: ExpressionStatics,
      ~fn_info: Info.exp,
      ~ancestors as _: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  open S;
  let (ty_in, ty_out) = MatchedTyp.arrow_tolerant(ctx, fn_info.ty);
  let (arg, _, m) = uexp_to_info_map(~ctx, ~ana=ty_in, arg, m);

  switch (Typ.normalize(ctx, arg.ty).term) {
  | Prod(entries) =>
    let entries =
      Util.OptUtil.traverse(Typ.match_tup_optional_label, entries);
    switch (entries) {
    | Some(entries) =>
      let val_typs = List.map(snd, entries);
      let joined_typ =
        Util.OptUtil.fold_left_opt(
          (acc, t) => Typ.meet(ctx, acc, t),
          val_typs,
          unknown,
        )
        |> Option.value(~default=unknown);

      add(
        ~syn_ty=
          IdTagged.FreshGrammar.Typ.(
            list(
              prod([
                tup_label(label("label"), string()),
                tup_label(label("value"), joined_typ),
              ]),
            )
          ),
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      );
    | _ =>
      add(
        ~syn_ty=ty_out,
        ~marks=[BuiltinError(ToLvsMissingLabelsOnTuple(ty_out))],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      )
    };
  | Unknown(_) =>
    add(
      ~syn_ty=ty_out,
      ~marks=[],
      ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
      m,
    )
  | _ =>
    add(
      ~syn_ty=ty_out,
      ~marks=[BuiltinError(ToLvsMissingLabelsOnTuple(ty_out))],
      ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
      m,
    )
  };
};

let omit_all_labels_statics =
    (
      module S: ExpressionStatics,
      ~fn_info: Info.exp,
      ~ancestors as _: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  S.(
    let (ty_in, ty_out) = MatchedTyp.arrow_tolerant(ctx, fn_info.ty);
    let (arg, _, m) = uexp_to_info_map(~ctx, ~ana=ty_in, arg, m);

    switch (Typ.normalize(ctx, arg.ty).term) {
    | Prod(entries) =>
      let entries =
        List.map(
          (e: Typ.t) =>
            switch (e.term) {
            | TupLabel(_, typ) => typ
            | _ => e
            },
          entries,
        );

      add(
        ~syn_ty=Typ.to_product(entries),
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      );
    | Unknown(_) =>
      add(
        ~syn_ty=ty_out,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      )
    | _ =>
      add(
        ~syn_ty=unknown,
        ~marks=[BuiltinError(ArgumentMustBeTuple)],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      )
    };
  );
};

let validate_label_arguments =
    (
      module S: ExpressionStatics,
      ~ctx: Ctx.t,
      ~expected_labels: option(list(string))=?,
      args: list(Exp.t),
      m: Map.t,
    )
    : (list(option(string)), Map.t) => {
  labels_to_info_map((module S), ~ctx, ~expected_labels?, args, m);
};

let analyze_args_syn =
    (module S: ExpressionStatics, ~ctx: Ctx.t, args, m: Map.t) =>
  map_m(
    (arg, m) =>
      S.uexp_to_info_map(~ctx, ~ana=syn, arg, m)
      |> (((info, _, m)) => (info, m)),
    args,
    m,
  );

let custom_statics_deferred_ap =
    (
      ~ctx: Ctx.t,
      ~ancestors as _,
      ~fn_info: Info.exp,
      kind: Ctx.custom_statics,
      module S: ExpressionStatics,
      m: Map.t,
      args: list(Exp.t),
    ) => {
  S.(
    switch (kind, args) {
    | (ProjectLabels | SelectLabels | OmitLabels, [tup, ...labels])
        when List.length(labels) > 0 =>
      let (tup_info, _, m) = uexp_to_info_map(~ctx, ~ana=syn, tup, m);
      let (_, m) = validate_label_arguments((module S), ~ctx, labels, m);

      add(
        ~syn_ty=Arrow(unknown, unknown) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
        m,
      );

    | (GroupByLabel, [table, pivot_label]) =>
      let (table_info, _, m) = uexp_to_info_map(~ctx, ~ana=syn, table, m);
      let (_, m) =
        validate_label_arguments((module S), ~ctx, [pivot_label], m);

      add(
        ~syn_ty=unknown,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, table_info.co_ctx]),
        m,
      );

    | (ToLvs | OmitAllLabels, [arg]) =>
      let (arg_info, _, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);

      add(
        ~syn_ty=unknown,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg_info.co_ctx]),
        m,
      );

    // Arity error cases
    | (ProjectLabels | SelectLabels | OmitLabels, [])
    | (ProjectLabels | SelectLabels | OmitLabels, [_]) =>
      let (args_info, m) = analyze_args_syn((module S), ~ctx, args, m);
      let combined_co_ctx =
        List.fold_left(
          (acc, info) => CoCtx.union([acc, Info.exp_co_ctx(info)]),
          fn_info.co_ctx,
          args_info,
        );

      add(
        ~syn_ty=unknown,
        ~marks=[BuiltinError(AtLeast2Arguments)],
        ~co_ctx=combined_co_ctx,
        m,
      );

    | (GroupByLabel, [])
    | (GroupByLabel, [_])
    | (GroupByLabel, [_, _, ..._]) =>
      let (args_info, m) = analyze_args_syn((module S), ~ctx, args, m);
      let combined_co_ctx =
        List.fold_left(
          (acc, info) => CoCtx.union([acc, Info.exp_co_ctx(info)]),
          fn_info.co_ctx,
          args_info,
        );

      add(
        ~syn_ty=unknown,
        ~marks=[BuiltinError(Exactly2Arguments)],
        ~co_ctx=combined_co_ctx,
        m,
      );

    // Fallback for other cases (including to_lvs/omit_all_labels with wrong arity)
    | _ =>
      let (args_info, m) = analyze_args_syn((module S), ~ctx, args, m);
      let combined_co_ctx =
        List.fold_left(
          (acc, info) => CoCtx.union([acc, Info.exp_co_ctx(info)]),
          fn_info.co_ctx,
          args_info,
        );

      let ty_in' =
        List.filter(e => Exp.is_deferral(e), args)
        |> List.map(_ => unknown)
        |> Typ.to_product;

      add(
        ~syn_ty=Arrow(ty_in', unknown) |> Typ.temp,
        ~marks=[],
        ~co_ctx=combined_co_ctx,
        m,
      );
    }
  );
};

let custom_statics_ap = (kind: Ctx.custom_statics) => {
  switch (kind) {
  | ProjectLabels => project_labels_statics
  | GroupByLabel => group_by_label_statics
  | ToLvs => to_lvs_statics
  | SelectLabels => select_labels_statics
  | OmitLabels => omit_labels_statics
  | OmitAllLabels => omit_all_labels_statics
  };
};
