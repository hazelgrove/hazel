open StaticsBase;

// TODO[Matt]: I'm not sure I've gotten elaboration right on here.

type tuple_entry =
  | Unlabeled(Typ.t)
  | Labeled(option(string), Typ.t);
type tuple_type = list(tuple_entry);

// Constants and helper functions
let unknown = Unknown(Internal) |> Typ.temp;
let syn = Unknown(SynSwitch) |> Typ.temp;
/* Build the elab `Ap` for a builtin call, preserving the source `Ap`'s
   annotation. The probe pipeline keys targets by the user_term ids stored
   in info_map, while the evaluator looks up those targets by the elab
   term's id; a fresh id here would cause the lookup to miss and probes
   on builtin calls would never fire. */
let mk_builtin_ap_elab =
    (~annotation: IdTagged.IdTag.t, fn_info: Info.exp, arg_elab: Exp.t): Exp.t => {
  term: Ap(Forward, fn_info.elab_term, arg_elab),
  annotation,
};

let append_marks_for_term = append_mark_exp;

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
    let m = append_mark_exp(m, arg_info.user_term, [error_override]);
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
  let m =
    switch (label.term) {
    | Label(name) =>
      /* `uexp_to_info_map` defaults Label(name) to UnexpectedLabelSort with
         elab_syn_ty=Unknown(Internal) because most occurrences of a bare label
         are wrong. In label position, the correct self type is Label(name);
         clear the mark AND patch the synthesized type so the cursor inspector
         shows Label(name) rather than Unknown(Internal). */
      let m = set_marks_exp(m, label, []);
      let m = patch_elab_syn_ty_exp(m, label, Label(name) |> Typ.temp);
      set_label_sort_exp(m, label, true);
    | EmptyHole => set_label_sort_exp(m, label, true)
    | _ =>
      append_mark_exp(m, label, [BadLabel(Exp(label))])
      |> set_label_sort_exp(_, label, true)
    };
  (
    lab_name,
    {
      ...i,
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
          let m =
            set_marks_exp(
              m,
              lab_info.user_term,
              lab_info.marks @ [InvalidLabel(name, expected)],
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
    (
      module S: ExpressionStatics,
      ~annotation: IdTagged.IdTag.t,
      ~ctx,
      ~fn_info: Info.exp,
      ~error,
      m,
      arg,
    ) => {
  S.(
    let (arg_info, arg_elab, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);
    add(
      ~elab_term=mk_builtin_ap_elab(~annotation, fn_info, arg_elab),
      ~elab_syn_ty=unknown,
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
      ~annotation: IdTagged.IdTag.t,
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
      /* Ensure all source tuple nodes get baseline info entries before
         specialized builtin tuple/label analysis rewrites parts of the arg. */
      let (_, arg_elab, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);
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
            elab_syn_ty: args_typ,
            marks: [],
            ty: fixed_typ(ctx, syn, args_typ),
            ana: syn,
            message: Message.Exp(Message.Default),
            warnings: [],
            ctx,
            co_ctx: CoCtx.empty,
            probe_targets: SubexpProbeTargets.empty,
            ancestors,
            user_term: arg,
            elab_term: arg,
            label_inference: None,
            inferred_label: None,
            label_sort: false,
            dot_labels: [],
          }),
          m,
        );

      let result_type = compute_result_type(labeled_tup_info, labels);
      add(
        ~elab_term=mk_builtin_ap_elab(~annotation, fn_info, arg_elab),
        ~elab_syn_ty=result_type,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
        m,
      );
    | _ =>
      invalid_args_fallback(
        (module S),
        ~annotation,
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
      ~annotation: IdTagged.IdTag.t,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  handle_tuple_operation(
    (module S),
    ~annotation,
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
      ~annotation: IdTagged.IdTag.t,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  handle_tuple_operation(
    (module S),
    ~annotation,
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
      ~annotation: IdTagged.IdTag.t,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  handle_tuple_operation(
    (module S),
    ~annotation,
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
      ~annotation: IdTagged.IdTag.t,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  S.(
    switch (arg.term) {
    | Tuple([table, pivot_label]) =>
      let (_, arg_elab, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);
      let (row_info: option(tuple_type), table_info, m) =
        analyze_table_argument((module S), ~ctx, m, table);

      let expected_labels = Option.map(extract_labels, row_info);
      let (label, _, _, m) =
        analyze_label_to_info_map((module S), ~ctx, syn, pivot_label, m);
      let m =
        switch (pivot_label.term, expected_labels) {
        | (Label(name), Some(expected)) when !List.mem(name, expected) =>
          append_mark_exp(m, pivot_label, [InvalidLabel(name, expected)])
        | _ => m
        };

      let m =
        Map.add_info(
          arg.annotation.ids,
          InfoExp({
            cls: Cls.Exp(Exp.cls_of_term(arg.term)),
            elab_syn_ty: Prod([table_info.ty, unknown]) |> Typ.temp,
            marks: [],
            ty:
              fixed_typ(
                ctx,
                syn,
                Prod([table_info.ty, unknown]) |> Typ.temp,
              ),
            ana: syn,
            message: Message.Exp(Message.Default),
            warnings: [],
            ctx,
            co_ctx: CoCtx.empty,
            probe_targets: SubexpProbeTargets.empty,
            ancestors,
            user_term: arg,
            elab_term: arg,
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
        ~elab_term=mk_builtin_ap_elab(~annotation, fn_info, arg_elab),
        ~elab_syn_ty=unknown,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, table_info.co_ctx]),
        m,
      );
    | _ =>
      invalid_args_fallback(
        (module S),
        ~annotation,
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
      ~annotation: IdTagged.IdTag.t,
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
        ~elab_term=mk_builtin_ap_elab(~annotation, fn_info, arg.elab_term),
        ~elab_syn_ty=
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
        ~elab_term=mk_builtin_ap_elab(~annotation, fn_info, arg.elab_term),
        ~elab_syn_ty=ty_out,
        ~marks=[BuiltinError(ToLvsMissingLabelsOnTuple(ty_out))],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      )
    };
  | Unknown(_) =>
    add(
      ~elab_term=mk_builtin_ap_elab(~annotation, fn_info, arg.elab_term),
      ~elab_syn_ty=ty_out,
      ~marks=[],
      ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
      m,
    )
  | _ =>
    add(
      ~elab_term=mk_builtin_ap_elab(~annotation, fn_info, arg.elab_term),
      ~elab_syn_ty=ty_out,
      ~marks=[BuiltinError(ToLvsMissingLabelsOnTuple(ty_out))],
      ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
      m,
    )
  };
};

let omit_all_labels_statics =
    (
      module S: ExpressionStatics,
      ~annotation: IdTagged.IdTag.t,
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
        ~elab_term=mk_builtin_ap_elab(~annotation, fn_info, arg.elab_term),
        ~elab_syn_ty=Typ.to_product(entries),
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      );
    | Unknown(_) =>
      add(
        ~elab_term=mk_builtin_ap_elab(~annotation, fn_info, arg.elab_term),
        ~elab_syn_ty=ty_out,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      )
    | _ =>
      add(
        ~elab_term=mk_builtin_ap_elab(~annotation, fn_info, arg.elab_term),
        ~elab_syn_ty=unknown,
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
        ~elab_syn_ty=Arrow(unknown, unknown) |> Typ.temp,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
        m,
      );

    | (GroupByLabel, [table, pivot_label]) =>
      let (table_info, _, m) = uexp_to_info_map(~ctx, ~ana=syn, table, m);
      let (_, m) =
        validate_label_arguments((module S), ~ctx, [pivot_label], m);

      add(
        ~elab_syn_ty=unknown,
        ~marks=[],
        ~co_ctx=CoCtx.union([fn_info.co_ctx, table_info.co_ctx]),
        m,
      );

    | (ToLvs | OmitAllLabels, [arg]) =>
      let (arg_info, _, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);

      add(
        ~elab_syn_ty=unknown,
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
        ~elab_syn_ty=unknown,
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
        ~elab_syn_ty=unknown,
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
        ~elab_syn_ty=Arrow(ty_in', unknown) |> Typ.temp,
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
