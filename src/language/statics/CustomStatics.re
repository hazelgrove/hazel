open StaticsBase;

type tuple_entry =
  | Unlabeled(Typ.t)
  | Labeled(option(string), Typ.t);
type tuple_type = list(tuple_entry);

// Constants and helper functions
let unknown = Unknown(Internal) |> Typ.temp;
let syn = Unknown(SynSwitch) |> Typ.temp;

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
      ~error_override,
      ~extract_entries,
      m,
      arg,
    ) => {
  open S;
  let (arg_info, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);

  switch (extract_entries(Typ.normalize(ctx, arg_info.ty))) {
  | Success(entries) => (
      Some(List.map(typ_entry_to_tuple_entry, entries)),
      arg_info,
      m,
    )
  | Unknown => (None, arg_info, m)
  | Failure =>
    let (_, m) =
      uexp_to_info_map(~ctx, ~ana=syn, ~override_self=error_override, arg, m);
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

let labels_to_info_map =
    (
      module S: ExpressionStatics,
      expected_labels: option(list(string)),
      labs: list(Exp.t),
      m: Map.t,
    )
    : (list(option(string)), Map.t) => {
  List.fold_left(
    ((labels: list(option(string)), m: Map.t), label) => {
      let (label, _, m) =
        S.label_to_info_map(expected_labels, syn, label, m);
      (labels @ [label], m);
    },
    ([], m),
    labs,
  );
};

// Common fallback for invalid arguments
let invalid_args_fallback =
    (module S: ExpressionStatics, ~ctx, ~fn_info: Info.exp, ~error, m, arg) => {
  S.(
    let (arg_info, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);
    add'(
      ~self=error,
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
        labels_to_info_map((module S), expected_labels, labs, m);

      let args_typ =
        Typ.to_product([tup_info.ty] @ List.map(__ => unknown, labs));

      let m =
        add_info(
          arg.annotation.ids,
          InfoExp(
            Info.derived_exp(
              ~uexp=arg,
              ~ctx,
              ~ana=syn,
              ~ancestors,
              ~self=Common(Just(args_typ)),
              ~co_ctx=CoCtx.empty,
              ~label_inference=None,
              ~inferred_label=None,
              ~label_sort=false,
              ~rewrite_id=None,
            ),
          ),
          m,
        );

      let result_type = compute_result_type(labeled_tup_info, labels);
      add'(
        ~self=Common(Just(result_type)),
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
      ~inferred_label as _,
      ~label_sort as _,
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
      ~inferred_label as _,
      ~label_sort as _,
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
      ~inferred_label as _,
      ~label_sort as _,
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
      ~inferred_label as _,
      ~label_sort as _,
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
      let (label, _, m) =
        label_to_info_map(expected_labels, syn, pivot_label, m);

      let m =
        add_info(
          arg.annotation.ids,
          InfoExp(
            Info.derived_exp(
              ~uexp=arg,
              ~ctx,
              ~ana=syn,
              ~co_ctx=CoCtx.empty,
              ~ancestors,
              ~self=
                Common(Just(Prod([table_info.ty, unknown]) |> Typ.temp)),
              ~label_inference=None,
              ~inferred_label=None,
              ~label_sort=false,
              ~rewrite_id=None,
            ),
          ),
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
          uexp_to_info_map(
            ~ctx,
            ~label_sort=true,
            ~override_self=BuiltinError(PivotLabelIsNotString(ty)),
            pivot_label,
            m,
          )
          |> snd
        | _ => m
        };

      add'(
        ~self=Common(Just(unknown)),
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
      ~inferred_label as _,
      ~label_sort as _,
      ~fn_info: Info.exp,
      ~ancestors as _: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  open S;
  let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn_info.ty);
  let (arg, m) = uexp_to_info_map(~ctx, ~ana=ty_in, arg, m);

  switch (Typ.normalize(ctx, arg.ty).term) {
  | Prod(entries) =>
    let entries =
      Util.OptUtil.traverse(Typ.match_tup_optional_label, entries);
    switch (entries) {
    | Some(entries) =>
      let val_typs = List.map(snd, entries);
      let joined_typ =
        Util.OptUtil.fold_left_opt(
          (acc, t) => Typ.join(ctx, acc, t),
          val_typs,
          unknown,
        )
        |> Option.value(~default=unknown);

      add'(
        ~self=
          Common(
            Just(
              IdTagged.FreshGrammar.Typ.(
                list(
                  prod([
                    tup_label(label("label"), string()),
                    tup_label(label("value"), joined_typ),
                  ]),
                )
              ),
            ),
          ),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      );
    | _ =>
      add'(
        ~self=BuiltinError(ToLvsMissingLabelsOnTuple(ty_out)),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      )
    };
  | Unknown(_) =>
    add'(
      ~self=Common(Just(ty_out)),
      ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
      m,
    )
  | _ =>
    add'(
      ~self=BuiltinError(ToLvsMissingLabelsOnTuple(ty_out)),
      ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
      m,
    )
  };
};

let omit_all_labels_statics =
    (
      module S: ExpressionStatics,
      ~inferred_label as _,
      ~label_sort as _,
      ~fn_info: Info.exp,
      ~ancestors as _: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  S.(
    let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn_info.ty);
    let (arg, m) = uexp_to_info_map(~ctx, ~ana=ty_in, arg, m);

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

      add'(
        ~self=Common(Just(Typ.to_product(entries))),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      );
    | Unknown(_) =>
      add'(
        ~self=Common(Just(ty_out)),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      )
    | _ =>
      add'(
        ~self=BuiltinError(ArgumentMustBeTuple),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
        m,
      )
    };
  );
};

let validate_label_arguments =
    (
      module S: ExpressionStatics,
      ~ctx as _: Ctx.t,
      expected_labels: option(list(string)),
      args: list(Exp.t),
      m: Map.t,
    )
    : (list(option(string)), Map.t) => {
  List.fold_left(
    ((labels: list(option(string)), m: Map.t), arg) => {
      let (label, _, m) = S.label_to_info_map(expected_labels, syn, arg, m);
      (labels @ [label], m);
    },
    ([], m),
    args,
  );
};

let custom_statics_deferred_ap =
    (
      ~inferred_label as _,
      ~label_sort as _,
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
      let (tup_info, m) = uexp_to_info_map(~ctx, ~ana=syn, tup, m);
      let (_, m) =
        validate_label_arguments((module S), ~ctx, None, labels, m);

      add'(
        ~self=Common(Just(Arrow(unknown, unknown) |> Typ.temp)),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
        m,
      );

    | (GroupByLabel, [table, pivot_label]) =>
      let (table_info, m) = uexp_to_info_map(~ctx, ~ana=syn, table, m);
      let (_, m) =
        validate_label_arguments((module S), ~ctx, None, [pivot_label], m);

      add'(
        ~self=Common(Just(unknown)),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, table_info.co_ctx]),
        m,
      );

    | (ToLvs | OmitAllLabels, [arg]) =>
      let (arg_info, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);

      add'(
        ~self=Common(Just(unknown)),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg_info.co_ctx]),
        m,
      );

    // Arity error cases
    | (ProjectLabels | SelectLabels | OmitLabels, [])
    | (ProjectLabels | SelectLabels | OmitLabels, [_]) =>
      let (args_info, m) =
        List.fold_left(
          ((acc_info, acc_m), arg) => {
            let (info, new_m) =
              S.uexp_to_info_map(~ctx, ~ana=syn, arg, acc_m);
            (acc_info @ [info], new_m);
          },
          ([], m),
          args,
        );
      let combined_co_ctx =
        List.fold_left(
          (acc, info) => CoCtx.union([acc, Info.exp_co_ctx(info)]),
          fn_info.co_ctx,
          args_info,
        );

      add'(
        ~self=BuiltinError(AtLeast2Arguments),
        ~co_ctx=combined_co_ctx,
        m,
      );

    | (GroupByLabel, [])
    | (GroupByLabel, [_])
    | (GroupByLabel, [_, _, ..._]) =>
      let (args_info, m) =
        List.fold_left(
          ((acc_info, acc_m), arg) => {
            let (info, new_m) =
              S.uexp_to_info_map(~ctx, ~ana=syn, arg, acc_m);
            (acc_info @ [info], new_m);
          },
          ([], m),
          args,
        );
      let combined_co_ctx =
        List.fold_left(
          (acc, info) => CoCtx.union([acc, Info.exp_co_ctx(info)]),
          fn_info.co_ctx,
          args_info,
        );

      add'(
        ~self=BuiltinError(Exactly2Arguments),
        ~co_ctx=combined_co_ctx,
        m,
      );

    // Fallback for other cases (including to_lvs/omit_all_labels with wrong arity)
    | _ =>
      let (args_info, m) =
        List.fold_left(
          ((acc_info, acc_m), arg) => {
            let (info, new_m) =
              S.uexp_to_info_map(~ctx, ~ana=syn, arg, acc_m);
            (acc_info @ [info], new_m);
          },
          ([], m),
          args,
        );
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

      add'(
        ~self=Common(Just(Arrow(ty_in', unknown) |> Typ.temp)),
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
