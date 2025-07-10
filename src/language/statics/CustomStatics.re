open StaticsBase;

type tuple_type = list((option(string), Typ.t));
let analyze_tuple_argument = (module S: ExpressionStatics, ~ctx, m, tup) => {
  open S;

  let (tup_info, m) =
    uexp_to_info_map(~ctx, ~ana=Unknown(SynSwitch) |> Typ.temp, tup, m);

  switch (Typ.normalize(ctx, tup_info.ty).term) {
  | Prod(entries) => (
      Some(
        List.map(
          (entry: Typ.t) => {
            switch (entry.term) {
            | TupLabel({term: Label(l), _}, typ) => (Some(l), typ)
            | TupLabel(_, typ) => (None, typ)
            | _ => (None, entry)
            }
          },
          entries,
        ),
      ),
      tup_info,
      m,
    )
  | Unknown(_) => (None, tup_info, m)
  | _ =>
    let (_, m) =
      uexp_to_info_map(
        ~ctx,
        ~ana=Unknown(SynSwitch) |> Typ.temp,
        ~override_self=BuiltinError(ArgumentMustBeTuple),
        tup,
        m,
      );
    (None, tup_info, m);
  };
};

let analyze_table_argument = (module S: ExpressionStatics, ~ctx, m, table) => {
  open S;

  let (table_info, m) =
    uexp_to_info_map(~ctx, ~ana=Unknown(SynSwitch) |> Typ.temp, table, m);

  switch (Typ.normalize(ctx, table_info.ty).term) {
  | List({term: Prod(entries), _}) => (
      Some(
        List.map(
          (entry: Typ.t) => {
            switch (entry.term) {
            | TupLabel({term: Label(l), _}, typ) => (Some(l), typ)
            | TupLabel(_, typ) => (None, typ)
            | _ => (None, entry)
            }
          },
          entries,
        ),
      ),
      table_info,
      m,
    )
  | Unknown(_) => (None, table_info, m)
  | _ =>
    let (_, m) =
      uexp_to_info_map(
        ~ctx,
        ~ana=Unknown(SynSwitch) |> Typ.temp,
        ~override_self=BuiltinError(ArgumentMustBeListOfTuples),
        table,
        m,
      );
    (None, table_info, m);
  };
};

let extract_labels = (entries: tuple_type) =>
  List.filter_map(
    (entry: (option(string), Typ.t)) => fst(entry),
    entries,
  );

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
        S.label_to_info_map(
          expected_labels,
          Unknown(SynSwitch) |> Typ.temp,
          label,
          m,
        );
      (labels @ [label], m);
    },
    ([], m),
    labs,
  );
};

let get_tuple_label = (tuple: tuple_type, label: string): Typ.t => {
  switch (List.find_opt(((l, _)) => l == Some(label), tuple)) {
  | Some((_, typ)) => typ
  | None => Unknown(Internal) |> Typ.temp
  };
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
  S.(
    switch (arg.term) {
    | Tuple([tup, ...labs]) when List.length(labs) > 0 =>
      let (labeled_tup_info: option(tuple_type), tup_info, m: Map.t) =
        analyze_tuple_argument((module S), ~ctx, m, tup);

      let expected_labels = Option.map(extract_labels, labeled_tup_info);

      let (labels, m) =
        labels_to_info_map((module S), expected_labels, labs, m);
      let args_typ =
        Typ.to_product(
          [tup_info.ty]
          @ List.map(__ => Unknown(Internal) |> Typ.temp, labs),
        );

      let m =
        add_info(
          arg.annotation.ids,
          InfoExp(
            Info.derived_exp(
              ~uexp=arg,
              ~ctx,
              ~ana=Unknown(SynSwitch) |> Typ.temp,
              ~ancestors,
              ~self=Common(Just(args_typ)),
              ~co_ctx=CoCtx.empty,
              ~label_inference=None,
              ~inferred_label=None,
              ~label_sort=false,
            ),
          ),
          m,
        );

      let val_types =
        List.map(
          (optional_lab: option(string)) => {
            Util.OptUtil.map2(get_tuple_label, labeled_tup_info, optional_lab)
            |> Option.value(~default=Unknown(Internal) |> Typ.temp)
          },
          labels,
        );

      add'(
        ~self=Common(Just(Typ.to_product(val_types))),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
        m,
      );
    | _ =>
      let (arg_info, m) =
        uexp_to_info_map(~ctx, ~ana=Unknown(SynSwitch) |> Typ.temp, arg, m);
      add'(
        ~self=BuiltinError(AtLeast2Arguments),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg_info.co_ctx]),
        m,
      );
    }
  );
};

let primitive_pivot_statics =
    (
      module S: ExpressionStatics,
      ~inferred_label,
      ~label_sort,
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
        label_to_info_map(
          expected_labels,
          Unknown(SynSwitch) |> Typ.temp,
          pivot_label,
          m,
        );

      let m =
        add_info(
          arg.annotation.ids,
          InfoExp(
            Info.derived_exp(
              ~uexp=arg,
              ~ctx,
              ~ana=Unknown(SynSwitch) |> Typ.temp,
              ~ancestors,
              ~self=
                Common(
                  Just(
                    Prod([table_info.ty, Unknown(Internal) |> Typ.temp])
                    |> Typ.temp,
                  ),
                ),
              ~co_ctx=CoCtx.empty,
              ~label_inference=None,
              ~inferred_label,
              ~label_sort,
            ),
          ),
          m,
        );
      let pivot_type =
        Util.OptUtil.map2(
          (entries: list((option(string), TermBase.typ_t)), label: string) => {
            List.find_map(
              ((l: option(string), ty: Typ.t)) =>
                l == Some(label) ? Some(ty) : None,
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
        ~self=Common(Just(Unknown(Internal) |> Typ.temp)),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, table_info.co_ctx]),
        m,
      );
    | _ =>
      let (arg_info, m) = uexp_to_info_map(~ctx, ~ana=syn, arg, m);
      add'(
        ~self=BuiltinError(Exactly2Arguments),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg_info.co_ctx]),
        m,
      );
    }
  );
};

let melt_statics =
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
          Unknown(Internal) |> Typ.temp,
        )
        |> Option.value(~default=Unknown(Internal) |> Typ.temp);

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
        ~self=BuiltinError(MeltMissingLabelsOnTuple(ty_out)),
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
      ~self=BuiltinError(MeltMissingLabelsOnTuple(ty_out)),
      ~co_ctx=CoCtx.union([fn_info.co_ctx, arg.co_ctx]),
      m,
    )
  };
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
  S.(
    switch (arg.term) {
    | Tuple([tup, ...labs]) when List.length(labs) > 0 =>
      let (labeled_tup_info: option(tuple_type), tup_info, m: Map.t) =
        analyze_tuple_argument((module S), ~ctx, m, tup);

      let expected_labels = Option.map(extract_labels, labeled_tup_info);

      let (labels, m) =
        labels_to_info_map((module S), expected_labels, labs, m);
      let args_typ =
        Typ.to_product(
          [tup_info.ty]
          @ List.map(__ => Unknown(Internal) |> Typ.temp, labs),
        );

      let m =
        add_info(
          arg.annotation.ids,
          InfoExp(
            Info.derived_exp(
              ~uexp=arg,
              ~ctx,
              ~ana=Unknown(SynSwitch) |> Typ.temp,
              ~ancestors,
              ~self=Common(Just(args_typ)),
              ~co_ctx=CoCtx.empty,
              ~label_inference=None,
              ~inferred_label=None,
              ~label_sort=false,
            ),
          ),
          m,
        );

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
            |> Option.value(~default=Unknown(Internal) |> Typ.temp)
          },
          labels,
        );

      add'(
        ~self=Common(Just(Typ.to_product(val_types))),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
        m,
      );
    | _ =>
      let (arg_info, m) =
        uexp_to_info_map(~ctx, ~ana=Unknown(SynSwitch) |> Typ.temp, arg, m);
      add'(
        ~self=BuiltinError(AtLeast2Arguments),
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg_info.co_ctx]),
        m,
      );
    }
  );
};
let omit_labels_statics =
    (
      module S: ExpressionStatics,
      ~inferred_label,
      ~label_sort,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  S.(
    switch (arg.term) {
    | Tuple([tup, ...labs]) =>
      let (tup_info, m) =
        uexp_to_info_map(~ctx, ~ana=Unknown(SynSwitch) |> Typ.temp, tup, m);

      let (labels, m) =
        List.fold_left(
          ((labels: list(Info.exp), m: Map.t), label) => {
            let (_, label_info, m) =
              label_to_info_map(
                None,
                Unknown(SynSwitch) |> Typ.temp,
                label,
                m,
              );
            (labels @ [label_info], m);
          },
          ([], m),
          labs,
        );

      let m =
        add_info(
          arg.annotation.ids,
          InfoExp(
            Info.derived_exp(
              ~uexp=arg,
              ~ctx,
              ~ana=Unknown(SynSwitch) |> Typ.temp,
              ~ancestors,
              ~self=Common(Just(Unknown(Internal) |> Typ.temp)),
              ~co_ctx=CoCtx.empty,
              ~label_inference=None,
              ~inferred_label,
              ~label_sort,
            ),
          ),
          m,
        );

      switch (Typ.normalize(ctx, tup_info.ty).term) {
      | Prod(entries) =>
        let labels =
          List.map(
            (label: Info.exp) =>
              switch (label.ty.term) {
              | Label(l) => Some(l)
              | _ => None
              },
            labels,
          );

        let entries = List.filter_map(Typ.match_tup_optional_label, entries);

        let missing_labels =
          List.filter_map(
            (label: option(string)) => {
              switch (label) {
              | Some(label) =>
                switch (
                  List.find_opt(((l, _)) => l == Some(label), entries)
                ) {
                | Some(_) => None
                | None => Some(label)
                }
              | None => None
              }
            },
            labels,
          );

        let val_types =
          List.filter_map(
            ((label: option(string), typ: Typ.t)) =>
              switch (label) {
              | Some(label) when !List.mem(Some(label), labels) =>
                Some(TupLabel(Label(label) |> Typ.temp, typ) |> Typ.temp)
              | Some(_) => None
              | None =>
                Some(
                  TupLabel(Unknown(Internal) |> Typ.temp, typ) |> Typ.temp,
                )
              },
            entries,
          );

        let self: Self.exp =
          switch (missing_labels) {
          | [] => Common(Just(Prod(val_types) |> Typ.temp))
          | _ => BuiltinError(ProjectLabelsMissingLabels(missing_labels)) // Better error message this is labels not found
          };

        add'(
          ~self,
          ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
          m,
        );
      | Unknown(_) =>
        let labels =
          List.map(
            (label: Info.exp) =>
              switch (label.ty.term) {
              | Label(l) => Some(l)
              | _ => None
              },
            labels,
          );

        let val_types =
          List.map(_ => {Unknown(Internal) |> Typ.temp}, labels);

        let self: Self.exp =
          Common(
            Just(
              switch (val_types) {
              | [x] => x
              | _ => Prod(val_types) |> Typ.temp
              },
            ),
          );

        add'(~self, ~co_ctx=fn_info.co_ctx, m);
      | _ =>
        let (_, m) =
          uexp_to_info_map(
            ~ctx,
            ~ana=Unknown(SynSwitch) |> Typ.temp,
            ~override_self=BuiltinError(ArgumentMustBeTuple),
            tup,
            m,
          );
        add'(
          ~self=Common(Just(Unknown(Internal) |> Typ.temp)), // Consider if there's a better way to show no type information
          ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
          m,
        );
      };
    | _ =>
      let (arg_info, m) =
        uexp_to_info_map(
          ~ctx,
          ~ana=Unknown(SynSwitch) |> Typ.temp,
          ~override_self=BuiltinError(ArgumentMustBeTuple),
          arg,
          m,
        );
      add'(
        ~self=Common(Just(Unknown(Internal) |> Typ.temp)), // Consider if there's a better way to show no type information
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg_info.co_ctx]),
        m,
      );
    }
  );
};

let drop_labels_statics =
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
        ~self=
          Common(Just(IdTagged.FreshGrammar.Typ.(list(prod(entries))))),
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

let custom_statics_ap = (kind: Ctx.custom_statics) => {
  switch (kind) {
  | ProjectLabels => project_labels_statics
  | PrimitivePivot => primitive_pivot_statics
  | Melt => melt_statics
  | SelectLabels => select_labels_statics
  | OmitLabels => omit_labels_statics
  | DropLabels => drop_labels_statics
  };
};
