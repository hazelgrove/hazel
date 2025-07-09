open StaticsBase;

let project_labels_statics =
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
    | Tuple([tup, ...labs]) when List.length(labs) > 0 =>
      let (
        labeled_tup_info: option(list((option(string), Typ.t))),
        tup_info,
        m: Map.t,
      ) = {
        let (tup_info, m) =
          uexp_to_info_map(
            ~ctx,
            ~ancestors,
            ~ana=Unknown(SynSwitch) |> Typ.temp,
            tup,
            m,
          );

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
              ~ancestors,
              ~override_self=BuiltinError(ArgumentMustBeTuple),
              tup,
              m,
            );
          (None, tup_info, m);
        };
      };

      let expected_labels =
        switch (labeled_tup_info) {
        | Some(entries) =>
          Some(
            List.filter_map(
              (entry: (option(string), Typ.t)) => fst(entry),
              entries,
            ),
          )
        | None => None
        };

      let (labels, m) =
        List.fold_left(
          ((labels: list(option(string)), m: Map.t), label) => {
            let (label, _, m) =
              label_to_info_map(
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

      let val_types =
        List.map(
          optional_lab => {
            switch (optional_lab) {
            | Some(l: string) =>
              List.find_map(
                ((entry_lab: option(string), ty)) =>
                  Some(l) == entry_lab ? Some(ty) : None,
                labeled_tup_info |> Option.value(~default=[]),
              )
              |> Option.value(~default=Unknown(Internal) |> Typ.temp)
            | None => Unknown(Internal) |> Typ.temp
            }
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
        uexp_to_info_map(
          ~ctx,
          ~ana=Unknown(SynSwitch) |> Typ.temp,
          ~ancestors,
          arg,
          m,
        );
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
    | Tuple([tup, pivot_label]) =>
      let (tup_info, m) =
        uexp_to_info_map(
          ~ancestors,
          ~ctx,
          ~ana=List(Unknown(SynSwitch) |> Typ.temp) |> Typ.temp,
          tup,
          m,
        );

      let (_, label_info, m) =
        label_to_info_map(
          None,
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
      | List({term: Prod(entries), _}) =>
        let pivot_label =
          switch (label_info.ty.term) {
          | Label(l) => Some(l)
          | _ => None
          };

        let entries: list((string, Grammar.typ_t(IdTagged.IdTag.t))) =
          List.filter_map(Typ.match_tup_label, entries);

        let pivot_entry: option((string, Grammar.typ_t(IdTagged.IdTag.t))) =
          List.find_opt(
            ((l, _): (string, Grammar.typ_t(IdTagged.IdTag.t))) =>
              Some(l) == pivot_label,
            entries,
          );

        let self: Self.exp =
          switch (pivot_entry, pivot_label) {
          | (_, None) => Common(Just(Unknown(Internal) |> Typ.temp)) // No pivot label provided
          | (None, Some(pivot_label)) =>
            BuiltinError(MissingLabels([pivot_label]))
          | (Some((_, typ)), _) =>
            switch (Typ.normalize(ctx, typ).term) {
            | Atom(String) => Common(Just(Unknown(Internal) |> Typ.temp)) // Happy path
            | Unknown(_) => Common(Just(Unknown(Internal) |> Typ.temp)) // No type information
            | _ => BuiltinError(PivotLabelIsNotString(typ)) // Pivot label not a string
            }
          };

        add'(
          ~self,
          ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
          m,
        );
      | Unknown(_) =>
        let self: Self.exp = Common(Just(Unknown(Internal) |> Typ.temp));

        add'(~self, ~co_ctx=fn_info.co_ctx, m);
      | _ =>
        let (_, m) =
          uexp_to_info_map(
            ~ctx,
            ~ana=Unknown(SynSwitch) |> Typ.temp,
            ~ancestors,
            ~override_self=BuiltinError(PivotFirstArgNotListOfTuples),
            tup,
            m,
          );
        add'(
          ~self=Common(Just(Unknown(Internal) |> Typ.temp)), // Consider if there's a better way to show no type information
          ~co_ctx=CoCtx.union([fn_info.co_ctx, tup_info.co_ctx]),
          m,
        );
      };
    | Tuple(_) =>
      let (arg_info, m) =
        uexp_to_info_map(
          ~ctx,
          ~ana=
            Prod([
              List(Unknown(Internal) |> Typ.temp) |> Typ.temp,
              Unknown(Internal) |> Typ.temp,
            ])
            |> Typ.temp,
          ~ancestors,
          arg,
          m,
        );
      add'(
        ~self=Common(Just(Unknown(Internal) |> Typ.temp)), // Consider if there's a better way to show no type information
        ~co_ctx=CoCtx.union([fn_info.co_ctx, arg_info.co_ctx]),
        m,
      );
    | _ =>
      let (arg_info, m) =
        uexp_to_info_map(
          ~ctx,
          ~ana=Unknown(SynSwitch) |> Typ.temp,
          ~ancestors,
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

let melt_statics =
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
  open S;
  let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn_info.ty);

  let (arg, m) = uexp_to_info_map(~ancestors, ~ctx, ~ana=ty_in, arg, m);

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
      ~inferred_label,
      ~label_sort,
      ~fn_info: Info.exp,
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  S.(
    // TODO Combine with ProjectLabels
    switch (arg.term) {
    | Tuple([tup, ...labs]) =>
      let (tup_info, m) =
        uexp_to_info_map(
          ~ctx,
          ~ancestors,
          ~ana=Unknown(SynSwitch) |> Typ.temp,
          tup,
          m,
        );

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

        let entries = List.filter_map(Typ.match_tup_label, entries);

        let (missing_labels, val_types) =
          List.partition_map(
            (label: option(string)) => {
              switch (label) {
              | Some(label) =>
                switch (List.find_opt(((l, _)) => l == label, entries)) {
                | Some((_, typ)) =>
                  Right(TupLabel(Label(label) |> Typ.temp, typ) |> Typ.temp)
                | None => Left(label)
                }
              | None => Right(Unknown(Internal) |> Typ.temp)
              }
            },
            labels,
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
            ~ancestors,
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
          ~ancestors,
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
        uexp_to_info_map(
          ~ctx,
          ~ancestors,
          ~ana=Unknown(SynSwitch) |> Typ.temp,
          tup,
          m,
        );

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
            ~ancestors,
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
          ~ancestors,
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
      ~ancestors: list(Id.t),
      ~ctx: Ctx.t,
      m: Map.t,
      arg: Exp.t,
    ) => {
  S.(
    let (ty_in, ty_out) = Typ.matched_arrow(ctx, fn_info.ty);

    let (arg, m) = uexp_to_info_map(~ctx, ~ancestors, ~ana=ty_in, arg, m);

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
